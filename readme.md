README for the validation checks of EEMs data
=============================================

## Checks I want
1. Check the absorbance curve of the tea standards vs. an average of the known good absorbance spectra. Bubbles can really affect the absorbance spectra, and this test will help us know if there is a bubble issue in the aqualog.

2. Check the absorbance of the blanks. Same as the tea standards this will help us know that the blank check vials are behaving as expected and not affected by bubbles.

3. Create an EEM Tea average or baseline for comparison and then compare the EEMs of a tea standard to that baseline. This could help with bubble issues or show how the lamp intensity has changed or the tea standard has changed (due to a bad batch?). 



Maybe - can I amalgamate all the known stream water DOC samples to create a known range of absorbance and EEMs for these? This will be more work than doing the tea and blanks.

## TODOs
-[x] make the writing to processing_tracking.txt it's own function to make code easier to read
-[] Figure out what eem_interp is doing. 
  - maybe I can use this function to find the spikes in the EEMs? or at least it might help think about what 3D eem processing steps can help with the validation
-[] add blank plotting and force user to accept before processing
-[] create a model of blank absorbance and fluorescence from good data



## Qs for Katie
- why drop S290-350 from calculations?
- Should I remove all data below the 1st order Rayleigh line? 
