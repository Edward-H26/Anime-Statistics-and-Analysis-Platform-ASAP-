# Anime Statistics and Analysis Platform (ASAP)

## Authors

Qiran Hu ([qiranhu2\@illinois.edu](mailto:qiranhu2@illinois.edu))\
Zoey Yao ([wanjing4\@illinois.edu](mailto:wanjing4@illinois.edu))

## Link To Our Presentation

<https://uofi.box.com/s/h2lxjv17jtb5sndd9jh81x91fl7izf3q>

## Project Proposal

This project aims to develop a Shiny website application that integrates with the anime Data API to analyze the popularity trend of anime. Interactive visualizations and prediction analysis will be implemented. The application will provide valuable insights for researchers and the general public, enabling them to better understand public's taste on anime.

## From Subculture To Mainstream: Examining Anime's Transformation From Microculture Into Global Mainstream Through User Engagements

# Abstract

The anime culture has become a global phenomenon that significantly influences popular culture and shapes the entertainment industry worldwide with skyrocketed revenues and unprecedented international growth in recent years. Streaming services in particular have significantly improved anime across the world and fostered a gigantic fanbase globally, which is expected to contribute "a compound annual growth rate of 10.20% from 2023 to 2030"in international licensing and merchandising (Bali, 2025). This paper will examine the key factors of anime's global market expansion through "anime content creations and distributions" and analyze their implications for stakeholders and investors as well as regular anime fans (Shinde, 2024). Thus, this project aims to provide content creators, investors, and anime fans with a deeper understanding of anime's expanding economic footprint and "significant investment opportunities" as anime characters and seriesemerge in this global industry (Shinde, 2024).

## Data

### Data Source

Through the [Jikan REST API](https://jikan.moe/), we can obtain the most updated data from the website, ensuring our application provides accurate and real-time information for all users.

### Key Dimensions

-   mal_id - Unique anime ID for each anime
-   title - name of the anime
-   type - TV, Movie, OVA
-   episodes - number of episodes for each anime
-   status - if the anime is currently airing or finished airing
-   rating - the rating of each anime such as PG13, R+, and PG-Children
-   score - the score of the anime based on a scale of 0-10
-   scored_by - number of users who scored the anime
-   rank - the rank of anime based on score
-   popularity - how popular is the anime
-   members - number of users who have watched the anime
-   favorites - number of users who added this anime to their favorites
-   genres - the specific style of anime
-   studios - the studios that made the anime
-   producers - the producers that made the anime

## Set Up

### Using Online Version 

If you prefer not to install anything, you can access the Anime Statistics and Analysis Platform directly through your web browser by clicking the following link. Since the application is hosted online through Shinyapps.io at, simply click that link and the app will load in your browser for immediate use. This is the easiest way to explore our platform because there are no setup, no downloads, and no technical steps needed. 

<https://qiranhu.shinyapps.io/AnimeStatisticsandAnalysisPlatform/>

### Running Our Application Locally 

If you want to run our platform on your own computer, then you can follow the step by step guide below. These instructions are designed to help users to set up our application on their devices locally. Since our system was developed using R version 4.1, we recommend using R 4.1+ for best compatibility. If you don't have R yet, you can download it for free from the official R Project website. While not strictly required, it's highly recommended to use RStudio which is a friendly interface for R that makes running Shiny apps easier. After setting up the environment, you can clone the repository located at <https://github.com/illinois-stat447/sp25-prj-qiranhu2-wanjing4> using the command line. This will create a local copy of the project folder on your machine.

Now, you are ready to launch the application. With the app.R file still open in RStudio, locate and click the run app button. It's usually a green triangle icon at the top of the source editor panel. By default, RStudio will show the app in a pop up window. You can also choose to open it in your external web browser by clicking the small drop down arrow next to Run App and selecting run external. Within a few seconds, you should see our application interface appear. Once the app is running, you can start using it interactively. The interface will be the same as the online version. You can navigate through the tabs, input search queries or parameters, and view various anime statistics and visualizations. When you finish exploring your favorite animes, you can simply close the app window or stop the Shiny app by clicking the red stop icon in RStudio's console panel. By following these instructions, you should be able to set up and run the Anime Statistics and Analysis Platform without any hassle. Thus, you can run the app anytime by just opening app.R in RStudio and clicking Run App because this process only needs to be done once.

If you are using the default database or other large database after fetching through the API key, it may take a while to run certain features since the predictive models or the data visulizations take some time to process. But it will not take more than 1 or 2 minutes. 

For more detailed analysis, you could explore the following Rmarkdown files to explore how we achieved each part of our project. 

To run Rmarkdown files, run in this order:

Data_Fetching -\> Data_Preprocessing_p1 -\> Data_Analysis -\> Data_Preprocessing_p2 -\> Predictive_Analysis

## Reference

Bali, V. (2025, March 19). The global anime market size was USD 28.8 billion in 2023!. Cognitive Market Research. <https://www.cognitivemarketresearch.com/anime-market-report>

Shinde, Y. (2024, July 3). Anime market trends: A journey towards USD 74.8 bn. Market.us Scoop. <https://scoop.market.us/anime-market-news/>

Dyck, S. O. P. R. (2024, September 6). Should Christians watch anime? A discernment guide. Pauline.org. <https://pauline.org/media-mindfulness/should-christians-watch-anime-a-discernment-guide/>

Razak, S. (2025, January 9). Exploring the world of Anime. prezi.com. <https://prezi.com/p/8jc6oob9utvq/exploring-the-world-of-anime/>

Ferjan, M. (2024, January 8). 20+ Anime Statistics & Facts: How many people watch anime? (2025). HeadphonesAddict. <https://headphonesaddict.com/anime-statistics/>

Pace-McCarrick, S. (2021, December 16). How far does anime challenge Joseph Nye's "Soft power" and its approach to culture? E-International Relation. <https://www.e-ir.info/2021/12/11/how-far-does-anime-challenge-joseph-nyes-soft-power-and-its-approach-to-culture/>

Iwabuchi, K. (2002, November 8). Recentering globalization. De Gruyter Brill. <https://www.degruyterbrill.com/document/doi/10.1515/9780822384083/html>

Fennell, D., Liberato, A. S. Q., Hayden, B., & Fujino, Y. (2013, September 1). Consuming anime. The Aquila Digital Community. <https://aquila.usm.edu/fac_pubs/8302/>

Mash, M. (2024, September 4). The global impact of japanese animation: How anime shaped pop culture. ALC Atlantis. <https://alc-atlantis.com/the-global-impact-of-japanese-animation-how-anime-shaped-pop-culture/>

Scottnicole, S. (2024, January 17). Psychological benefits of Cosplay. The Urban Crews. <https://www.theurbancrews.com/lifestyle/cosplay-psychological-benefits/>

Hulu - Anime News Network. (2025, April 26). Animenewsnetwork.com. <https://www.animenewsnetwork.com/encyclopedia/company.php?id=6901>

Tran, V. H., Sebastian, Y., Karim, A., & Azam, S. (2024, December 5). Distinguishing human journalists from artificial storytellers through stylistic fingerprints. MDPI. <https://www.mdpi.com/2073-431X/13/12/328>

 DeGuzman, K. (2024, April 17). Non-linear plots - how to write stories out of order. StudioBinder. <https://www.studiobinder.com/blog/what-is-a-non-linear-plot-definition/>

 Bischoff, M. (2024, February 20). How recommendation algorithms work--and why they may miss the mark. Scientific American. <https://www.scientificamerican.com/article/how-recommendation-algorithms-work-and-why-they-may-miss-the-mark/#:~:text=Whether%20we%E2%80%99re%20using%20Spotify%2C%20Amazon%2C,For%20example%2C%20the>

Peters, M. (2024, October 5). Here's how demon slayer's anime boosted the manga sales. ComicBook.com. <https://comicbook.com/anime/news/demon-slayer-anime-influence-manga-sales-boost/#:~:text=In%20terms%20of%20numbers%2C%20Demon,all%20thanks%20to%20the%20anime>

Atherton, M. T. (2024, April 18). Anime's outsized impact on social media, quantified. Anime Trending \| Your Voice in Anime! <https://www.anitrendz.com/news/2024/04/19/animes-outsized-impact-on-social-media-quantified>

Wikimedia Foundation. (2025, April 22). Statistics. Wikipedia. <https://en.wikipedia.org/wiki/Wikipedia:Statistics#:~:text=As%20of%20April%202025%2C%20the,encyclopedic%20text%20is%20added>

 Purini, R. (2024, July 3). Sony Group Portal - Crunchyroll's mission to be the ultimate destination for anime fans. Sony Group Portal - Home. <https://www.sony.com/en/SonyInfo/blog/2024/07/03/>
