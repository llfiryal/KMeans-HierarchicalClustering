# K Means - Hierarchical Clustering R Code

## Github Repository: https://github.com/llfiryal/KMeans-HierarchicalClustering
## GEE Data Source: https://code.earthengine.google.com/b82098481c3dcfa2046fe7fb05fcebbb

## Data Extraction from Google Earth Engine (GEE)
1. Open Google Earth Engine Code Editor.
2. Paste the provided GEE JavaScript code snippet (or access via https://code.earthengine.google.com/b82098481c3dcfa2046fe7fb05fcebbb).
3. Ensure the study area is set to the southern parts of Jakarta (already define in the code).
4. Download all CSV files to local computer.
5. For reproduction purpose, please use SpatioTemporal_NTL_Central_South_JKT_FIX.csv dataset available in this repository.

## Run the Code for Clustering and Analysis
1. Open RStudio
2. Install and load required packages
3. Adjust the file path to your local dataset in this line (Please use SpatioTemporal_NTL_Central_South_JKT_FIX.csv dataset)
4. Run the entire R script sequentially to:
-	Perform K-Means and Hierarchical Clustering
-	Generate cluster assignments
-	Calculate brightness summaries
-	Compute silhouette scores
-	Export clustering results to CSV
5. Output files generated:
-	Clustered Data Maps Produced by K-Means and Hierarchical Clustering
-	Cluster_Brightness_Ranges.csv (Cluster brightness ranges)
-	KMeans_Silhouette_2012.csv (Silhouette results for K-Means for 2012 data)
-	Hierarchical_Silhouette_2012.csv (Silhouette results for Hierarchical for 2012 data)
-	Cluster_Distribution_Per_Year.csv (Cluster point counts per year)
6. All figures and tables in the report (cluster maps, year-on-year change, silhouette scores, etc.) can be generated using the outputs of this code.
