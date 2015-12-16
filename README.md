# Tabloid

This repo contains a collection of code wrote during the "Data Analysis Marathon" i Krakow, 2015. The objective of the contest was to propose idea of "tabloidization index" of web portal articles. 

We built our solution in 3 stages process: 

1. Features extraction
2. Model learning
3. New articles evaluation

## Features extractions

We created the following features to describe each of web articles: 

- fraction of verbs
- fraction of punctuation
- fraction of numerals
- fraction of prepositions
- fraction of adjectives 
- fraction of adverbs
- fraction of nouns
- fraction of conjunctions
- fraction of other part of speech 
- is from FAKT
- is from PAP
- has image  
- has media gallery
- fraction of commas
- fraction of semicolons
- fraction of colons
- fraction of exclamations
- fraction of question marks
- fraction of dots 
- fraction of quotation marks
- fraction of suspension points
- fraction of 1-syllables words 
- fraction of 2-syllables words 
- fraction of 3-syllables words 
- fraction of 4-syllables words 
- fraction of 5 and more-syllables words 
- fraction of words we selected as "tabloid-related"
- fraction of words we selected as "non-tabloid-related"
- probability of being related to some particular topics that emerged from topic modelling apporach and which topics we selected as "tabloid-related"

## Model learning

We make use of the following modelling techniques: 

- Random Forest binary clarrification

# The cat() Team

The cat() Team are 4 people. 

![Alt text](http://i.imgur.com/cb5HoAG.jpg)



