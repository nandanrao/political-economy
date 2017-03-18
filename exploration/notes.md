## todo

## measures of pac spending
* cluster spending -- find media ad buying
* add pac/super-pac from opensecrets to spending
* add it to regression


## endogeneity!!!! (quality)
* candidate ideology (for distance with district)
* latent variable reresentation (get other measures -- combine with individual contribs) -- ideology + individual contribs + .... .?
* Strengthen argument of ideology score


## data wrangling
* group by id/year
* sum votes and vote per
* inc (max)
* name --> pick first

(data frame) %>%
    group_by(id, year) %>%
    summarise(name = first(name), votes = sum(votes), inc = max(inc))
