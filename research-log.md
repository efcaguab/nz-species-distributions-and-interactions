# Research log

*Fernando Cagua*

#### 16 October 2018

Had a look at the intro of [Dependence Modeling with Copulas by Harry Joe](https://www.crcpress.com/Dependence-Modeling-with-Copulas/Joe/p/book/9781466583221). Just gives an overview of what copulas are, which I already. Won't be using copula theory for the chapter, as it's just a trick to get the multivariate methods working on non-continuous data. So there goes 260 dollars wasted. I could have used that money so much better...

#### 15 October 2018

Spent most of the day reading Popovich paper. I understand it a bit better. 

Copulas are just a method that allows to specify models for marginal distributions separately from the dependence structure that links those distributions into a joint one. That's it. Copulas can mix variables that have different distributions because it all get translated to uniform margins between 0 and 1.

What popovich paper is about is that there are many methods of dependence modelling that are cool (like Gaussian graphical models, factor analysis, latent variables, etc.) but these only work in Gaussian data. Not the case when you have species counts or presence/absence. Her work allows us to use these cool methods in discrete data by using a copula and some tricks of expectation maximisation. She shows that in many cases this approach is better than using dependence modelling in Pearson residuals (what happens on joint species distributions?) or the methods adapted for discrete data (there aren't many). It takes more computational power though. 

Daniel said in a previous meeting that Copulas allow you to infer actual interactions from species associations. Not at all... Copulas estimate a the link between distributions. He was probably thinking about Gaussian graphical models, which give the conditional dependence of two variables (the dependence given that the dependence among other variables has been taken care of). 

Gaussian graphical models are nicely explained in YouTube and [Epskamp et al. “The Gaussian Graphical Model in Cross-Sectional and Time-Series Data.”](https://doi.org/10.1080/00273171.2018.1454823). 

Some open questions for the project: I was thinking that if species dependences encapsulate missing environmental variables, then there should be a relationship with latent variables? Latent variables should be able to pick up the missing covariates? Or to simplify dependence on other species? Lasso to fit models of species-species presence/absence?

#### 11 October 2018

Alison Barner has a cool paper checking whether co-occurrence patterns can be used to infer non-trophic interactions in some tidal communities. Answer is no. Very different results. See study [here](http://onlinelibrary.wiley.com/doi/10.1002/ecy.2133/full). Data is available though and perhaps could be used to explore the same question from a different angle, whether knowing the interactions is useful to infer species distributions or co-occurrence are better still. Her dataset is closely related to PISCO to be OK. It has shit tons of survey data from Alaska to California. Which might help to use large scale species distribution models. 

Questions for Warwick:

* What angle you think is more interesting -> whether to co-occurrence from joint species distributions models or -> whether species distribution models are improved more with real interaction data or inferred co-occurrence. *Answer: If one could do both directions it would be really cool story. Otherwise interactions -> SDM sounds good*
* Danger of too many species with SY-NZ? *Answer: Yes, and also many understudied species and many missing interactions*

#### 10 October 2018

Had a meeting with Jason. He also mentioned the Plant Sy-nz database but highlighted that it has mainly herbivore interactions. He suggested that an option could be to integrate the data from George Perry (that Lupe is using for her project) and that of [Ruffel and Didham](https://newzealandecology.org/nzje/3296.pdf). 

Perry's data seems to be a collection of bird and plant interactions. Ruffel and Dietham worked on predicting the effect of forest cover and trapping on bird populations. 

#### 7 October 2018

After writing an email to Warwick about potential databases for this work he replied:

* Plant Sy-NZ: https://plant-synz.landcareresearch.co.nz/. This has a lot of plant-herbivore interactions and some predator-prey and plant-pollinator data too. Unfortunately, it doesn't have any information on species distributions and probably misses a lot of interactions (I have noticed some missing that I've observed in the field).
* Eco-invertebase: This is a Plant and Food database that I was told about by one of my advisors, Barbara Barratt. It's not publicly available and I haven't requested to use it yet, but apparently it has a lot of trophic information for pest herbivores and biocontrol agents, maybe more... I was told to contact Jacqui Todd at PFR Auckland to ask for permission to use the data (Jacqui.todd@plantandfood.co.nz).
* NZFungi: https://nzfungi2.landcareresearch.co.nz/. This is a sweet database containing information on plant-fungi and plant-bacteria associations. Like Plant Sy-NZ, it also doesn't include information on species distributions and is likely to be even more undersampled. Jennifer Bufford, my officemate at Lincoln, has used this database more than me and could offer more advice on its benefits and pitfalls.
* National Vegetation Survey: https://nvs.landcareresearch.co.nz/. Perhaps this may also be useful to you, potentially to inform virtual plant communities? I've toyed with this idea but never actually attempted it. As you're probably aware there's lots of datasets such as this which would be available around the world.

Warwick also suggested to talk to William Godsoe, who has been thinking about similar problems before. See his link to his paper with Gravel [Integrating Biogeography with Contemporary Niche Theory](https://www-sciencedirect-com.ezproxy.canterbury.ac.nz/science/article/pii/S0169534717300836).

#### 4 October 2018

Had a meeting with Daniel. Regarding potential datasets that include data on species locations to construct a distribution model and real interactions between the species. He highlighted three possible sources:

* PISCO. The pacific intertidal database of sedentary things. Has data on co-occurrence and potentially also on interactions of species. Can ask Spencer Woods (who was here on visit, and used to work maintaining the database) and Alison Barner (who seems to have worked with data and approaches to co-occurrence).

* EOL/GBIF Interaction database. 

Also suggested to check [Martin Anderson R package on copulas](https://cran.r-project.org/web/packages/copula/copula.pdf). It has examples about fitting copulas in R
