#### Quarto

This directory contains sub-components used to create the main website pages.
"Includes" are quarto documents that are nested within an overarching document using Quarto's [`includes` shortcode](https://quarto.org/docs/authoring/includes.html), equivalent to Rmarkdown child documents. 
Each "includes" document creates an individual section of the dashboard that is ultimately rendered in `index.qmd`.

The structure of the included documents is:

-   `index.qmd` *...includes...*
    -   `./quarto/_organisation.qmd` *...includes...*
        -   `.../_weight.qmd`
        -   `.../_bmi.qmd`
        -   `.../_participants.qmd`

Note that the names of Quarto files below the index page are named with a prefix "`_`" to stop Quarto from rendering them as separate pages.
