HD3 Transfers
================

### Analyzing Patient Transfers in the Atlanta HD3 Area

In my work as a data analyst with a government agency, I'm given a dataset of digitized case reports of people who came in to private practice doctors, public hospitals, and nursing facilities. Some of these cases tested positive for a certain antibiotic-resistant infection. By tracking where these people went (transfers between hospitals), I can generate a network map of where this bug went. Here's a de-identified version of that map.

![Antibiotic-Resistant Transfers](Visuals/ARI%20Network.gif) [PDF Version](Visuals/ARI%20Network.pdf)

I also have a dataset of all of the patient transfers in the state of Georgia, regardless of whether or not they tested positive for this bug. If we can find a significant correlation between these maps, then we can use general patient transfer data in order to model the spread of the bug we care about.
