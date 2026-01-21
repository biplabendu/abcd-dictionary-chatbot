import pandas as pd
from sentence_transformers import SentenceTransformer, util
import torch

# Global variables
df = None
model = None
corpus_embeddings = None

def initialize_backend(csv_path):
    """
    Loads the CSV and pre-computes embeddings for the 'label' column.
    """
    global df, model, corpus_embeddings
    
    print(f"Initializing Python Backend with file: {csv_path}...")
    
    try:
        # 1. Load Data
        df = pd.read_csv(csv_path)
        
        # Ensure we search on strings
        search_texts = df['label'].fillna("").astype(str).tolist()

        # 2. Load Model
        model = SentenceTransformer('all-MiniLM-L6-v2')
        
        # 3. Pre-compute embeddings
        print("Generating embeddings... (this may take a moment)")
        corpus_embeddings = model.encode(search_texts, convert_to_tensor=True)
        print("Backend Ready.")
        
    except Exception as e:
        print(f"Error initializing backend: {e}")
        raise e

def semantic_search(search_string, cutoff=0.2):
    """
    Simulates: sentences_sorted[sims > cutoff]
    Calculates similarity for ALL rows, filters by cutoff, and sorts.
    """
    global df, model, corpus_embeddings
    
    if df is None or model is None:
        return pd.DataFrame()

    if not search_string or search_string.strip() == "":
        return df.head(0)

    # Encode user query
    query_embedding = model.encode(search_string, convert_to_tensor=True)

    # Perform Cosine Similarity (Compare query to ALL corpus embeddings)
    # Result is a tensor of shape (n_samples,)
    sims = util.cos_sim(query_embedding, corpus_embeddings)[0]

    # Convert to numpy for DataFrame operations
    sims_np = sims.cpu().numpy()
    
    # Create a working copy to return
    df_result = df.copy()
    df_result['similarity'] = sims_np
    
    # FILTER: equivalent to sentences[sims > cutoff]
    # We cast cutoff to float to ensure comparison works
    df_filtered = df_result[df_result['similarity'] > float(cutoff)]
    
    # SORT: descending order
    df_sorted = df_filtered.sort_values(by='similarity', ascending=False)
    
    return df_sorted

def get_unique_values(col_name):
    """
    Helper to get unique values for UI filters.
    """
    global df
    if df is not None and col_name in df.columns:
        return df[col_name].dropna().unique().tolist()
    return []

