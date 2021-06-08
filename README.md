# fft_and_spectral_plots_app
This repository introduces a simple but an effective ShinyApp about obtaning FFT and Spectral Density Plots easily.

* In app.R file, the codes are available. The output website can be found here: https://mrreductive.shinyapps.io/fft_and_spectral_plots/

In this app, there are various parameters that can be changed namely:

- Sampling Rate (Hz) : Sampling Rate (in Hz). Default is 20000.

- Harmonic Frequency Selection: Which frequency to search in both plots. Default is 300.

- Number of Harmonic to search: After assigning some value for the Harmonic Frequency Selection, you would get vertical red lines that you have specified for this parameter. Default is 3.

- Number of Freq for Spec.ar: For the spec.ar() function, it is the value for how many number of frequency to be generated. Default is 1000.

- AR Order for Spec.ar: For the spec.ar() function, the order of the AR(p) model. Default is p=100.


After opening the web application, all you need to do is upload a snapshot file and then changing the parameters in the bottom-left corner.

