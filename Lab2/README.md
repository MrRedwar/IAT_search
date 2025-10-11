# Основы обработки данных с помощью R и Dplyr
mironopavel@yandex.ru

##Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить практические навыки использования функций обработки данных
    пакета dplyr – функции select(), filter(), mutate(), arrange(),
    group_by()

## Исходные данные

1.  Программное обеспечение Windows 10 Pro
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.5.1
4.  Програмный пакет dplyr
5.  Набор данных starwars

## План

1.  Установить пакет dplyr  
2.  Выполнить курс

## Шаги

1 Установить пакет dplyr

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

1.  Проанализировать встроенный в пакет dplyr набор данных языка R и
    ответить на вопросы:

2.1 Сколько строк в датафрейме?

``` r
starwars %>% nrow()
```

    [1] 87

2.2 Сколько столбцов в датафрейме?

``` r
starwars %>% ncol()
```

    [1] 14

2.3 Как просмотреть примерный вид датафрейма?

``` r
starwars %>% glimpse()
```

    Rows: 87
    Columns: 14
    $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
    $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
    $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
    $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
    $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
    $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
    $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
    $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
    $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
    $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
    $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
    $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
    $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
    $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

2.4 Сколько уникальных рас персонажей (species) представлено в данных?

``` r
starwars$species %>% unique() %>% length()
```

    [1] 38

2.5 Найти самого высокого персонажа.

``` r
starwars %>% filter(height == max(height, na.rm = TRUE)) %>% select(name, height)
```

    # A tibble: 1 × 2
      name        height
      <chr>        <int>
    1 Yarael Poof    264

2.6 Найти всех персонажей ниже 170

``` r
starwars %>% filter(height < 170) %>% select(name, height)
```

    # A tibble: 22 × 2
       name                  height
       <chr>                  <int>
     1 C-3PO                    167
     2 R2-D2                     96
     3 Leia Organa              150
     4 Beru Whitesun Lars       165
     5 R5-D4                     97
     6 Yoda                      66
     7 Mon Mothma               150
     8 Wicket Systri Warrick     88
     9 Nien Nunb                160
    10 Watto                    137
    # ℹ 12 more rows

2.7 Подсчитать ИМТ (индекс массы тела) для всех персонажей. ИМТ
подсчитать по формуле

``` r
starwars <- starwars %>% mutate(imt = mass / ((height / 100) ^ 2))

starwars$imt
```

     [1]  26.02758  26.89232  34.72222  33.33007  21.77778  37.87401  27.54821
     [8]  34.00999  25.08286  23.24598  23.76641        NA  21.54509  24.69136
    [15]  24.72518 443.42857  26.64360  33.95062  39.02663  25.95156  23.35095
    [22]  35.00000  31.30194  25.21625  25.79592  25.61728        NA        NA
    [29]  25.82645  26.56250  23.89326  24.67038        NA  13.14828  17.18034
    [36]  16.34247        NA        NA        NA  31.88776        NA        NA
    [43]  26.12245        NA  17.35892  24.03461  50.92802        NA  24.46460
    [50]  23.76641  20.91623  22.64681        NA  14.76843        NA        NA
    [57]  22.63468        NA  24.83565        NA        NA  23.88844  19.44637
    [64]  18.14487        NA  21.47709        NA  23.58984  19.48696  26.01775
    [71]  16.78076        NA        NA        NA  12.88625        NA  17.99015
    [78]  34.07922  24.83746  22.35174  15.14960  18.85192        NA        NA
    [85]        NA        NA        NA

2.8 Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по
отношению массы (mass) к росту (height) персонажей.

``` r
starwars %>% mutate(stretch = mass / height) %>% arrange(desc(stretch)) %>% head(10) %>% select(name, mass, height, stretch)
```

    # A tibble: 10 × 4
       name                   mass height stretch
       <chr>                 <dbl>  <int>   <dbl>
     1 Jabba Desilijic Tiure  1358    175   7.76 
     2 Grievous                159    216   0.736
     3 IG-88                   140    200   0.7  
     4 Owen Lars               120    178   0.674
     5 Darth Vader             136    202   0.673
     6 Jek Tono Porkins        110    180   0.611
     7 Bossk                   113    190   0.595
     8 Tarfful                 136    234   0.581
     9 Dexter Jettster         102    198   0.515
    10 Chewbacca               112    228   0.491

2.9 Найти средний возраст персонажей каждой расы вселенной Звездных
войн.

``` r
 starwars %>% mutate(age = 100 + birth_year) %>% group_by(species) %>% summarise(avg_age = mean(age, na.rm = TRUE), count = n()) %>% filter(!is.na(avg_age)) %>% arrange(desc(avg_age))
```

    # A tibble: 15 × 3
       species        avg_age count
       <chr>            <dbl> <int>
     1 Yoda's species    996      1
     2 Hutt              700      1
     3 Wookiee           300      2
     4 Cerean            192      1
     5 Zabrak            154      2
     6 Human             154.    35
     7 Droid             153.     6
     8 Trandoshan        153      1
     9 Gungan            152      3
    10 Mirialan          149      2
    11 Twi'lek           148      2
    12 Rodian            144      1
    13 Mon Calamari      141      1
    14 Kel Dor           122      1
    15 Ewok              108      1

2.10 . Найти самый распространенный цвет глаз персонажей вселенной
Звездных войн.

``` r
starwars %>% count(eye_color, sort = TRUE) %>% head(1)
```

    # A tibble: 1 × 2
      eye_color     n
      <chr>     <int>
    1 brown        21

2.11 Подсчитать среднюю длину имени в каждой расе вселенной Звездных
войн.

``` r
starwars %>% mutate(name_length = nchar(name)) %>% group_by(species) %>% summarise(avg_name_length = mean(name_length, na.rm = TRUE))
```

    # A tibble: 38 × 2
       species   avg_name_length
       <chr>               <dbl>
     1 Aleena              12   
     2 Besalisk            15   
     3 Cerean              12   
     4 Chagrian            10   
     5 Clawdite            10   
     6 Droid                4.83
     7 Dug                  7   
     8 Ewok                21   
     9 Geonosian           17   
    10 Gungan              11.7 
    # ℹ 28 more rows

## Оценка результата

В результате лабораторной работы мы поняли как работают функции пакета
dpylr при работе с данными.

## Вывод

Таким образом, мы научились мы научились выполнять базовые функции
пакета dpylr.
