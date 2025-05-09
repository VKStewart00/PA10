# PA 10: Exploring the Star Wars Universe
Van, Cherie

***This task is complex. It requires many different types of abilities.
Everyone will be good at some of these abilities but nobody will be good
at all of them. In order to produce the best product possible, you will
need to use the skills of each member of your group.***

<!-- The person who who is going the farthest from CSUMB this summer starts as the Developer (typing and listening to instructions from the Coder)!  -->

## Goals for the Activity

- Apply methods of to use lists and iteration (using `purrr`) to extract
  data from various non-tabular data sets.  
- Create new data sets through the cleaning, organization, and joining
  of data from various sources  
- Create visualizations to explore the data  
- May the force be with you!

**THROUGHOUT THE Activity** be sure to follow the Style Guide by doing
the following:

- load the appropriate packages at the beginning of the Rmarkdown  
- use proper spacing  
- name all code chunks  
- comment at least once in each code chunk to describe why you made your
  coding decisions  
- add appropriate labels to all graphic axes

## Review: Extracting Information from Different Data Sets

Here is information about the fist 7 Star Wars films:

``` r
View(sw_films) 
```

We are going to explore the data contained in several lists similar to
this one (and the previously explored `sw_people`), combining skills
from all of our previous R code learning experiences.

How do the following two codes compare?

``` r
sw_films[[4]][["title"]]
```

    [1] "Revenge of the Sith"

``` r
sw_films %>% pluck(4,"title")
```

    [1] "Revenge of the Sith"

> The first is looking though the dataset and searching for the title in
> the fourth dataframe.

> The second is pulling the title from the fourth dataset.

Suppose we want to pull out just the titles as a character vector,
select the correct code (comment out the rest) to perform this action.
You may want to run each line of code one at a time (remember
`Ctrl + Enter` for Windows with your cursor on that line of code).

``` r
#comment out the incorrect codes
## 'sw_films %>% map("title")
sw_films %>% map_chr("title")
```

    [1] "A New Hope"              "Attack of the Clones"   
    [3] "The Phantom Menace"      "Revenge of the Sith"    
    [5] "Return of the Jedi"      "The Empire Strikes Back"
    [7] "The Force Awakens"      

``` r
## 'sw_films %>% map_dfc("title")
```

Suppose we want to apply a function to count the number of specific
kinds of ships and vehicles in our data

Notice that for each film, the “starships” vector contains links to
information on those starships (though note this data is out of date and
should be linked at swapi.dev, not swapi.co).

``` r
sw_films[[1]][["starships"]]
```

    [1] "http://swapi.co/api/starships/2/"  "http://swapi.co/api/starships/3/" 
    [3] "http://swapi.co/api/starships/5/"  "http://swapi.co/api/starships/9/" 
    [5] "http://swapi.co/api/starships/10/" "http://swapi.co/api/starships/11/"
    [7] "http://swapi.co/api/starships/12/" "http://swapi.co/api/starships/13/"

So if we can count the number of webpage links that would tell us the
number of starships that appear in that movie. Here are three different
ways to count the number of urls under `starships`. Can you think of
another? (it is ok if you can’t). Compare and contrast how the three
codes work differently to do the same thing.

``` r
sw_films %>% map(., "starships")%>% map_dbl(~length(.))
```

    [1]  8  9  5 12 12  9  2

``` r
map_dbl(sw_films, ~length(.x$starships))
```

    [1]  8  9  5 12 12  9  2

``` r
sw_films %>% map_dbl(., ~length(.x$starships))
```

    [1]  8  9  5 12 12  9  2

``` r
## Yes we could
sw_films %>% map_int(., ~length(.x$starships))
```

    [1]  8  9  5 12 12  9  2

> The first line looks at all values under ‘starship’ across the
> dataset, then pulls the length

> The second, loads in all the values then pulls starship values and
> lengths.

> The third, same as above but the df is piped in.

<!-- Swap roles -- Developer becomes Coder, Coder becomes Developer! -->

## Part 1: Evaluating Hyperdrive in the Star Wars Episodes

We will use the third method from the previous section to extract out
the information we want from `sw_films`. For each row, specify if we
should use a regular `map()`, `map_dbl()`, or `map_chr()`.

**NOTE** Sometimes code like this gets a little finicky in R if you try
to run it with `Ctrl + Enter`. Instead, use the code chunk green arrow
to run the whole code chunk or highlight all of the code and then use
the shortcut to run it.

``` r
sw_ships_1 <- sw_films %>% {
  tibble(
    title = map_chr(., "title"), #character
    episode = map_dbl(., "episode_id"), #numeric
    starships = map_dbl(., ~length(.x$starships)), #numeric
    vehicles = map_dbl(., ~length(.x$vehicles)), #numeric
    planets = map_dbl(., ~length(.x$planets)) #numeric
  )}
sw_ships_1
```

    # A tibble: 7 × 5
      title                   episode starships vehicles planets
      <chr>                     <dbl>     <dbl>    <dbl>   <dbl>
    1 A New Hope                    4         8        4       3
    2 Attack of the Clones          2         9       11       5
    3 The Phantom Menace            1         5        7       3
    4 Revenge of the Sith           3        12       13      13
    5 Return of the Jedi            6        12        8       5
    6 The Empire Strikes Back       5         9        6       4
    7 The Force Awakens             7         2        0       1

Let’s do a bit more data cleaning to 1) assign the Trilogy
classification to each episode, 2) calculate the total number of
starships (which have hyperdrive) and vehicles (which do not have
hyperdrive), and 3) calculate the proportion of total ships that have
hyperdrive. Fill in the missing codes.

``` r
sw_ships <- sw_ships_1 %>%  
  #create a new variable called trilogy
  mutate(trilogy = case_when(episode %in% 1:3 ~ trilogies[1],
                             episode %in% 4:6 ~ trilogies[2],
                             episode %in% 7 ~ trilogies[3])) %>% 
  #create a new variable called total_ships which adds vehicles and starships together
  mutate(total_ships = vehicles + starships) %>%  
  #create a new variable called prop that calculate the percent hyperdrive
  mutate(prop = starships / total_ships * 100) 

sw_ships
```

    # A tibble: 7 × 8
      title             episode starships vehicles planets trilogy total_ships  prop
      <chr>               <dbl>     <dbl>    <dbl>   <dbl> <fct>         <dbl> <dbl>
    1 A New Hope              4         8        4       3 Origin…          12  66.7
    2 Attack of the Cl…       2         9       11       5 Preque…          20  45  
    3 The Phantom Mena…       1         5        7       3 Preque…          12  41.7
    4 Revenge of the S…       3        12       13      13 Preque…          25  48  
    5 Return of the Je…       6        12        8       5 Origin…          20  60  
    6 The Empire Strik…       5         9        6       4 Origin…          15  60  
    7 The Force Awakens       7         2        0       1 Sequel…           2 100  

### Hyperdrive Use Across Films

Now, let’s make a plot examining how often hyperdrive ships appear in
each episode. Fill in the blanks withe appropriate functions.

``` r
sw_ships %>% 
  #be sure to order titles by order/episode
  ggplot(aes(y = fct_reorder(title, desc(episode)), 
             x = prop)) + 
  #we want bars but our data is already summarized!
  geom_col(aes(fill = trilogy)) + 
  labs(
    title = "The Rise of Hyperdrive",
    subtitle = "Percentage of Ships with Hyperdrive Capability"
  ) +
  #you may need to install `scales` package if you haven't already
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  theme_minimal() +
  #what aesthetic do we modify to change the bar color
  scale_color_viridis_d(end = 0.8) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())
```

![](pa-10-star-wars-student_files/figure-commonmark/unnamed-chunk-10-1.png)

#### Canvas Quiz Question 1

Which movie has the second highest percentage of Hyperdrive ships?

> A New Hope

<!-- Swap roles -- Developer becomes Coder, Coder becomes Developer! -->

### Hyperdrive Prevalence within the Universe

We can also look at a plot to see if there is a correlation between the
total number of ships and the number with hyperdrive (starships). Fill
in the blanks withe appropriate functions.

``` r
sw_ships %>% 
  ggplot(aes(x = total_ships, 
             y = starships)) +
  #make points
  geom_point(aes(color = trilogy)) +
  #fit a model
  geom_smooth(method = "lm") +
  #what does geom_text() do?
  geom_text(aes(label = title), 
            vjust = -1, 
            hjust = "inward", 
            size = 2.75) +
  labs(title = "Hyperdrive Correlations",
       subtitle = "The Number of Ships with Hyperdrive vs Total Ships") +
  theme_minimal() +
  #what aesthetic do we want to modify the color of points?
  scale_color_viridis_d(end = 0.8) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) 
```

    `geom_smooth()` using formula = 'y ~ x'

![](pa-10-star-wars-student_files/figure-commonmark/unnamed-chunk-11-1.png)

#### Canvas Quiz Question 2

What do you notice about the use of hyperdrive type vehicles in the
episodes?

> The rate of hyperdrive ships increase over time.

## Part 2: The Physical Features of Star Wars Characters

Recall the data for “people” in Star Wars:

``` r
View(sw_people)
```

We want to extract out `name`, `height`, and `mass` as `character`
vectors (for now, we have to deal with some issues in height and weight
later to change them into double type vectors) and keep `films` as a
list for now. Fill in the correct `map` type functions for each one.

``` r
sw_peeps <- sw_people %>%  {
  tibble(
    name = map_chr(., "name"),  #character
    height = map_chr(., "height"), #character
    mass = map_chr(., "mass"), #character
    films = map(., "films") #list
  )}
sw_peeps
```

    # A tibble: 87 × 4
       name               height mass  films    
       <chr>              <chr>  <chr> <list>   
     1 Luke Skywalker     172    77    <chr [5]>
     2 C-3PO              167    75    <chr [6]>
     3 R2-D2              96     32    <chr [7]>
     4 Darth Vader        202    136   <chr [4]>
     5 Leia Organa        150    49    <chr [5]>
     6 Owen Lars          178    120   <chr [3]>
     7 Beru Whitesun lars 165    75    <chr [3]>
     8 R5-D4              97     32    <chr [1]>
     9 Biggs Darklighter  183    84    <chr [1]>
    10 Obi-Wan Kenobi     182    77    <chr [6]>
    # ℹ 77 more rows

Notice that the `films` column contains lists of urls for each film
reference. Let’s pull out that same information from the `sw_films` data
to have the `title` of the episode and the `url` as a `character`
vector, and the episode number as a numeric value. Fill in the correct
`map` type functions.

``` r
film_names <- sw_films %>% {
  tibble(
    episode_id = map_dbl(., "episode_id"), #double
    episode_name = map_chr(., "title"), #character
    url = map_chr(., "url") #character
  )}
film_names
```

    # A tibble: 7 × 3
      episode_id episode_name            url                         
           <dbl> <chr>                   <chr>                       
    1          4 A New Hope              http://swapi.co/api/films/1/
    2          2 Attack of the Clones    http://swapi.co/api/films/5/
    3          1 The Phantom Menace      http://swapi.co/api/films/4/
    4          3 Revenge of the Sith     http://swapi.co/api/films/6/
    5          6 Return of the Jedi      http://swapi.co/api/films/3/
    6          5 The Empire Strikes Back http://swapi.co/api/films/2/
    7          7 The Force Awakens       http://swapi.co/api/films/7/

<!-- Swap roles -- Developer becomes Coder, Coder becomes Developer! -->

Now we can finish cleaning up our data by doing the following:

1)  turn `height` and `mass` into numeric vectors;  
2)  match the `films`/`urls` to their `episode_names` and assign that
    back to `sw_peeps`.

``` r
sw_peeps2 <- sw_peeps %>% 
  #use a function from readr to extract the numbers and replace "unknown" with na
  mutate(height = parse_double(height, na = "unknown"),
         mass = parse_double(mass, na = "unknown")) %>%
  #unnest the lists in films
  unnest(cols = c("films")) %>% 
  #join the film data with episodes names to the people data
  left_join(film_names, by = c("films" = "url")) %>% 
  #remove the `films` url from the data frame
  select(-films) %>% 
  #add the variable trilogy
  mutate(trilogy = case_when(episode_id %in% 1:3 ~ trilogies[1],
                           episode_id %in% 4:6 ~ trilogies[2],
                           episode_id %in% 7   ~ trilogies[3]))
```

    Warning: There was 1 warning in `mutate()`.
    ℹ In argument: `mass = parse_double(mass, na = "unknown")`.
    Caused by warning:
    ! 1 parsing failure.
    row col               expected actual
     16  -- no trailing characters  1,358

``` r
sw_peeps2
```

    # A tibble: 173 × 6
       name           height  mass episode_id episode_name            trilogy       
       <chr>           <dbl> <dbl>      <dbl> <chr>                   <fct>         
     1 Luke Skywalker    172    77          3 Revenge of the Sith     Prequels: Epi…
     2 Luke Skywalker    172    77          6 Return of the Jedi      Originals: Ep…
     3 Luke Skywalker    172    77          5 The Empire Strikes Back Originals: Ep…
     4 Luke Skywalker    172    77          4 A New Hope              Originals: Ep…
     5 Luke Skywalker    172    77          7 The Force Awakens       Sequels: Epis…
     6 C-3PO             167    75          2 Attack of the Clones    Prequels: Epi…
     7 C-3PO             167    75          1 The Phantom Menace      Prequels: Epi…
     8 C-3PO             167    75          3 Revenge of the Sith     Prequels: Epi…
     9 C-3PO             167    75          6 Return of the Jedi      Originals: Ep…
    10 C-3PO             167    75          5 The Empire Strikes Back Originals: Ep…
    # ℹ 163 more rows

### Size of Characters in the Star Wars Universe

We can now create a plot of height and mass by trilogy group to see if
the physique of characters differed across Trilogies (keeping in mind
the third set of Trilogies is incomplete in this data set).

``` r
sw_peeps2 %>% 
  filter(name != "Jabba Desilijic Tiure") %>% #major outlier removed
  #map the correct aesthetics
  ggplot(aes(x = height, 
             y = mass, 
             color = trilogy))+
  geom_point(position = "jitter") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Height of Character (cm)",
       y = "Mass of Character (kg)",
       color = "Trilogy Group",
       title = "Character Characteristics in Star Wars") +
  theme_minimal() +
  scale_color_viridis_d(end = 0.8) 
```

    `geom_smooth()` using formula = 'y ~ x'

    Warning: Removed 36 rows containing non-finite outside the scale range
    (`stat_smooth()`).

    Warning: Removed 36 rows containing missing values or values outside the scale range
    (`geom_point()`).

![](pa-10-star-wars-student_files/figure-commonmark/unnamed-chunk-16-1.png)

#### Canvas Quiz Question 3

Write some code to identify who is is the heaviest (look at the graph to
help guide this) Star Wars character (excluding Jabba Desilijic Tiure).

``` r
sw_peeps2 %>% 
  filter(name != "Jabba Desilijic Tiure") %>% #major outlier removed
  #map the correct aesthetics
  ggplot(aes(x = height, 
             y = mass, 
             color = trilogy))+
  geom_point(position = "jitter") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Height of Character (cm)",
       y = "Mass of Character (kg)",
       color = "Trilogy Group",
       title = "Character Characteristics in Star Wars") +
  theme_minimal() +
  scale_color_viridis_d(end = 0.8)+
  #what does geom_text() do?
  geom_text(aes(label = name), 
            vjust = -1, 
            hjust = "inward", 
            size = 2)
```

    `geom_smooth()` using formula = 'y ~ x'

    Warning: Removed 36 rows containing non-finite outside the scale range
    (`stat_smooth()`).

    Warning: Removed 36 rows containing missing values or values outside the scale range
    (`geom_point()`).

    Warning: Removed 36 rows containing missing values or values outside the scale range
    (`geom_text()`).

![](pa-10-star-wars-student_files/figure-commonmark/unnamed-chunk-17-1.png)

> Grievous

**REMEMBER TO RENDER YOUR FINAL DOCUMENT**

<!-- Swap roles -- Developer becomes Coder, Coder becomes Developer! -->

## OPTIONAL CHALLENGE PROBLEM

Your professor wants to use `purrr` to try and generate a height and
mass scatterplot for each episode, but I don’t want to type out all that
code. Here is where I got so far, but I am not convince this is the most
sophisticated or effective way to do this. Do some research and see if
you can find a way to put this process into production!

``` r
plots_sw <- sw_peeps %>% 
  nest(data = !episode_name) %>% 
  mutate(plot = map(data, ~ggplot(., aes(y=mass, x=height)) + 
                      geom_point() + 
                      geom_smooth(method = "lm", se = FALSE) + 
                      labs(title = paste0(episode_name))))
```

    Error in `nest()`:
    ! In expression named `data`:
    Caused by error:
    ! Can't select columns that don't exist.
    ✖ Column `episode_name` doesn't exist.

``` r
print(plots_sw$plot)
```

    Error: object 'plots_sw' not found
