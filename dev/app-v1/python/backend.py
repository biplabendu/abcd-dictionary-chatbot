import pandas as pd
import numpy as np
from sentence_transformers import SentenceTransformer, util
from scipy.spatial import distance as ssd
from pathlib import Path


def create_embeddings(text_list, batch_size=64):
    # returns a numpy array by default
    model = SentenceTransformer("sentence-transformers/all-MiniLM-L6-v2")
    return model.encode(
        text_list,
        batch_size=batch_size,
        normalize_embeddings=True,
        show_progress_bar=True,
    )

def create_core_embeddings(csv_path, batch_size=64):
    """
    Loads the CSV and pre-computes embeddings for the 'label' column.
    """

    
    print(f"Initializing Python Backend with file: {csv_path}...")
    
    try:
        # 1. Load Data
        df = pd.read_csv(csv_path)
        df = df.dropna(subset=["label"])
        sentences = df['label'].values.tolist()
        
        # 3. Pre-compute embeddings
        embeddings = create_embeddings(sentences)
        # Save embeddings to file
        np.save("embeddings_minLLM_L6.npy", embeddings.astype("float32"))
    except Exception as e:
        print(f"Error creating embeddings: {e}")
        raise e

def semantic_search(search_string, cutoff=0.2):
    """
    Simulates: sentences_sorted[sims > cutoff]
    Calculates similarity for ALL rows, filters by cutoff, and sorts.
    """

    EMBEDDINGS_PATH = "/Users/bidas/Documents/GitHub/abcd-dictionary-chatbot/data/embeddings/embeddings_minLLM_L6.npy"
    embeddings = np.load(EMBEDDINGS_PATH)

    search_embeddings = create_embeddings([search_string,])
 
    sims, sentences_sorted, sorted_index = sentence_search(embeddings, search_embeddings)
 
    
    return sentences_sorted[:20].tolist()


def sentence_search(embeddings, search_embedding):
    # Compute cosine similarity scores for the search string to all other sentences
    CSV_PATH = "/Users/bidas/Documents/GitHub/abcd-dictionary-chatbot/data/dd-abcd-6_0_minimal_noimag.csv"
    df = pd.read_csv(CSV_PATH)
    df = df.dropna(subset=["label"])
    sentences = df['label'].values.tolist()
    sims = []
    for embedding in embeddings:
        sims.append(1 - ssd.cosine(search_embedding[0], embedding))
    # Sort sentences by similarity score in descending order (the most similar ones are first)
    sorted_index = np.argsort(sims)[::-1]
    sentences_sorted = np.array(sentences)[sorted_index]
    sims = np.array(sims)[sorted_index]
    return sims,  sentences_sorted, sorted_index


# if __name__ == "__main__":
#     sent = semantic_search("variables to compute body mass index")
#     print(sent)
