library(elastic)

# Specify Elasticsearch connection details
es_connection <- connect(
  hosts = "http://localhost:9200",
  user = NULL,
  passwd = NULL
)

# Index name and type
index_name <- "praias"
index_type <- "your_index_type"

# Create index if it doesn't exist
index_create(es_connection, index_name, body = list(mappings = list(properties = list(
  id = list(type = "integer"),
  name = list(type = "keyword"),
  age = list(type = "integer")
))))

# Bulk insert data into Elasticsearch
bulk(es_connection, index = index_name, type = index_type, records = df)
