# nz-species-distributions-and-interactions

## Reproduce the results 

### 1. Check requirements

Make sure that the software and hardware requirements (below) have been met. All code should everywhere that runs docker:

* Docker 18.09 
* 47 GB of RAM memory
* 50 GB free on disk

### 2. Get the code

Download or clone this repository in your computer using `git clone https://github.com/efcaguab/species-distributions-and-networks.git`.

### 3. Build and run the docker container

There is a couple alternatives here. The easiest one is using the following commands from the working directory.

```
docker build -t analysis-image .
docker run -d -e DISABLE_AUTH=true -p 8787:8787 -v $PWD:/home/rstudio/species-distributions-and-networks analysis-container analysis-image 
```

This will build the container image and run it for you. In some linux systems you will need sudo rights.

### 4. Get and store API credentials

This project depends on data from multiple online databases. Two of them NCBI and GBIF require API Keys or API authentication. 
For this project, API information should be specified in the `.Renviron` file.

```
ENTREZ_KEY='your_ncbi_api_key'
GBIF_USER='your_gbif_username'
GBIF_PWD='your_gbif_password'
GBIF_EMAIL='your_gbif_email'
```

Check this blog post with [instructions to obtain an NCBI ENTREZ_KEY](https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/). 

### 5. Run analyses

Just run the makefile in the project directory:

```
docker exec -u rstudio analysis-container make -C /home/rstudio/species-distributions-and-networks
```

