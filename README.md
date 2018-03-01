# Trip-Data-Visualization
An app to visualize reported trips made by individuals who participate in trip diary surveys.

You can find a working version of the app [here](https://moh-salah.shinyapps.io/Trip-Data-Visualization/).

The structure of the data and the header names have to match the enclosed sample dataset `fake_data.txt`. Trip routes are generated using Google Maps directions API which is accessed through the [googleway](https://cran.r-project.org/web/packages/googleway/vignettes/googleway-vignette.html) package. Users should set the variable `my_key` to their own Google API keys. At the time of developing this app (Feb 2018), only the `driving` and `walking` modes were supported by the `googleway` package. The 'walking' mode can take any of the following values: `Walk`, `walking`, or `on foot`. There are no requirements on the naming of the other modes, however, the `driving` mode will be used for any mode that is not `walking`.

I would love to hear your feedback and suggestions! You can contact me by [email](mahmoud.mohsalah@gmail.com) or through [LinkedIn](http://linkedin.com/in/moh-salah).

