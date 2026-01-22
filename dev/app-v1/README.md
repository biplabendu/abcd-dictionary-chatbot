# Running it on your computer

Once you have cloned the repo on your local computer, use the following instructions to run the app locally.

> Instructions below are based on a Mac running 

# Version requirements

- R version 4.5 and above [download here](https://cran.rstudio.com/)
- Rstudio version XX and above [download here](https://posit.co/download/rstudio-desktop/)
- python3
   - check if python3 is installed by typing `which python3` in terminal

## Steps to install necessary R and python libraries

* Install R packages
   * Double click the `app-v1.Rproj` file
   * Run `renv::restore()` to install necessary packages

* Install python libraries
    * Create a virtual env
       * `python3 -m venv python_env`
    * Activate the virtual env
       * `source python_env/bin/activate`
    * Install the libraries
       * `pip install -r requirements.txt`

## Run the app locally
* Re-open the R project, double-click the `app-v1.Rproj` file
* Open the `app.R` file 
* Click `Run App` at the top-right of the R script
