import pandas as pd
import numpy as np
from sentence_transformers import SentenceTransformer, util
from scipy.spatial import distance as ssd
from pathlib import Path
import sys

BACKEND_PATH = Path(sys.argv[0]).resolve().parent
EMBEDDINGS_PATH = BACKEND_PATH / "local_embeddings"
PROJECT_PATH = BACKEND_PATH.parent.parent.parent
print("PROJECT_PATH: ", PROJECT_PATH)

DATA_PATH = PROJECT_PATH / "data"
if not DATA_PATH.exists():
    raise FileNotFoundError(f"The project changed the structure and the data path not found: {DATA_PATH}")

SUPPORTED_MODELS = ["all-MiniLM-L6-v2", "all-MiniLM-L12-v2"]
DOMAINS_LIST = ['ABCD (General)','COVID-19','Endocannabinoid','Friends, Family, & Community','Genetics','Hurricane Irma','Imaging','Linked External Data','MR Spectroscopy','Mental Health','Neurocognition','Novel Technologies','Physical Health','Social Development','Substance Use']



def create_embeddings(csv_path, model, batch_size=64):
    """
    Loads the CSV and pre-computes embeddings for the questions from the chosen csv file.
    """

    # Load Data and get the sentences
    df = pd.read_csv(csv_path)
    df = df.dropna(subset=["label"])
    sentences = df['label'].values.tolist()
            
    # Encode the sentences
    return model.encode(
        sentences,
        batch_size=batch_size,
        normalize_embeddings=True,
        show_progress_bar=True,
    )

def create_search_embeddings(search_string, model, batch_size=64):
    """
    Creates embeddings for the search string.
    """
    # Encode the sentences
    return model.encode(
        [search_string,],
        batch_size=batch_size,
        normalize_embeddings=True,
        show_progress_bar=True,
    )


def semantic_search(search_string, domains_list=None, model_name="all-MiniLM-L6-v2", cutoff=0.2):
    """
    Simulates: sentences_sorted[sims > cutoff]
    Calculates similarity for ALL rows, filters by cutoff, and sorts.
    """

    if model_name in SUPPORTED_MODELS:
        model = SentenceTransformer(f"sentence-transformers/{model_name}")
    else:
        raise ValueError(f"Model {model_name} not supported. Supported models: {SUPPORTED_MODELS}")

    # assuming that the default is csv without imaging questions
    if domains_list is None:
        domains_list = DOMAINS_LIST
        domains_list.remove('Imaging')
        
    # if the domains list doesn't contain Imaging, then use the csv without imaging questions
    if  'Imaging' not in domains_list:
        csv_path =  DATA_PATH / "dd-abcd-6_0_minimal_noimag.csv"
        embeddings_name = f"embeddings_{model_name}_noimag.npy"
    else:
        csv_path =  DATA_PATH / "dd-abcd-6_0_minimal.csv"
        embeddings_name = f"embeddings_{model_name}.npy"

    # creating embedings for the questions if they don't exist
    if not (EMBEDDINGS_PATH / embeddings_name).exists() and not (BACKEND_PATH / embeddings_name).exists():
        embeddings = create_embeddings(csv_path, model)
        if not EMBEDDINGS_PATH.exists():
            print(f"Creating embeddings path: {EMBEDDINGS_PATH}")
            EMBEDDINGS_PATH.mkdir(parents=True, exist_ok=True)
        print(f"Saving embeddings to: {EMBEDDINGS_PATH / embeddings_name}")
        np.save(EMBEDDINGS_PATH / embeddings_name, embeddings.astype("float32"))
    elif (EMBEDDINGS_PATH / embeddings_name).exists():
        embeddings = np.load(EMBEDDINGS_PATH / embeddings_name)
    else:
        embeddings = np.load(BACKEND_PATH / embeddings_name)

    search_embeddings = create_search_embeddings(search_string, model)


    sims, sorted_index, sentences_sorted = sentence_search(csv_path, domains_list, embeddings, search_embeddings, cutoff)

    return sims, sorted_index, sentences_sorted


def sentence_search(csv_path, domains_list, embeddings, search_embedding, cutoff=0.2):
    # Compute cosine similarity scores for the search string to sentences for the given domains

    df = pd.read_csv(csv_path)
    df = df.dropna(subset=["label"])
    sentences = df['label'].values.tolist()
    # get the sentences for the given domains
    mask = df["domain"].isin(domains_list)
    
    sims = np.zeros(len(df), dtype=float)    
    for i in np.where(mask)[0]:
        sims[i] = 1 - ssd.cosine(search_embedding[0], embeddings[i])

    # Sort sentences by similarity score in descending order (the most similar ones are first)
    sorted_index = np.argsort(sims)[::-1]
    sentences_sorted = np.array(sentences)[sorted_index]
    sims = sims[sorted_index]
    return sims[sims > cutoff], sorted_index[sims > cutoff], sentences_sorted[sims > cutoff]


# if __name__ == "__main__":
#     sims, sorted_index, sentences_sorted = semantic_search("variables to compute body mass index")
#     print(sentences_sorted[:10])
