# --------------- #
# --- home UI --- #
# --------------- #

home_ui <- function(id){
  
  fluidPage(
    br(),
    br(),
    br(),
    HTML(
      '<h1 style="font-family:Georgia;color:black; width:75%; text-align:center;margin: 0 auto; font-weight: bold; font-size: 38px">
      NBA Hot Hand Analysis &#128293 </h1>'
    ),
    p(
      "By Matt Bolaños | matthew.a.bolanos@gmail.com |", a("Portfolio", href="https://www.mattbolanos.com/", target="_blank"),
      "|", a("Code", href="https://github.com/mattbolanos/TrackingHotHand", target="_blank"),
      style = "font-size:17px;font-family:Karla; color:black; width:75%; text-align:center; margin: 0 auto; "
    ),
    p(
      "Introduction",
      style = "font-size:24px; font-family:Georgia;color:black; width:75%; text-align:left;margin: 0 auto; font-weight: bold"
    ),
    br(),

    # Intro blurb ----------------------------------------------------------------------------------------------------------------------
    p(
      "This application summarizes and visualizes some of my research
             surrounding the infamous “Hot Hand Fallacy”. The fallacy stems from
             the concept that one is “hot” or “cold” based on past performance,
             even if it has no influence on future outcomes. In basketball,
             the Hot Hand is the idea that a player has an increased
             probability of making their next shot if they are “on fire”, i.e.,
             they have made their several previous shots.",
      style = "font-size:18px; font-family:Karla;color:black; width:75%; text-align:left;margin: 0 auto; "
    ),
    br(),
    p(
      "There have been numerous studies dedicated to investigating the
             Hot Hand, from the pioneers of Gilovich, Vallone & Tversky (1985)
             to more contemporary analyses by Miller & Sanjuro (2018). The former’s
             results were consistent with the long-standing notion that the
             percentage of shots made following a previous string of made shots
             is not significantly higher. The latter study concluded otherwise
             and provided evidence that the Hot Hand theory is statistically
             consistent with expectations. But Miller & Sanjuro’s Bayesian
             application is best used in random chance scenarios
             (e.g., flipping a coin), as opposed to skill-based performance
             employing muscle memory.",
      style = "font-size:18px; font-family:Karla;color:black; width:75%; text-align:left;margin: 0 auto; "
    ),
    br(),
    p(
      "I have previously analyzed NBA shot data from ESPN game archives to
             investigate player field goal percentages following strings
             of misses and makes. My work did not yield much evidence that shooters
             are statistically more likely to make their next shot after a string
             of makes or misses. But there was some minimal evidence of a
             “rubber band” effect, in which some of the better shooters in the
             league had a significantly greater chance of making their next shot
             after missing 3 or more shots in a row. Overall, my findings were
             consistent with that of Gilovich, Vallone & Tversky. Eighteen
             months later, after coming across some SportVU Game Logs from the
             2015-16 NBA Season, I decided to revisit my Hot Hand analysis.",
      style = "font-size:18px; font-family:Karla;color:black; width:75%; text-align:left;margin: 0 auto; "
    ),
    br(),
    # Tab 1 Blurb ----------------------------------------------------------------------------------------------------------------------
    p(
      "Tab 1: 2015-2021 Shot Charts by Streak",
      style = "font-size:24px; font-family:Georgia;color:black; width:75%; text-align:left;margin: 0 auto; font-weight: bold"
    ),
    br(),
    p(
      "The first tab of this application is a visualization tool that 
             displays how players shoot relative to league average, or their own averages, 
             from every spot on the court, following a particular streak. To keep bins from 
             getting too small (players don’t often make 7 shots in a row), 
             I categorized shots as being one of 1, 2, or 3+, Makes or Misses. 
             In an attempt to compare apples to apples, players are compared to 
             league average after the same streak, rather than the base league 
             average (e.g. Player X after 3+ Makes is compared to how the league 
             shot after 3+ Makes). The base eFG% computed when comparing players
             against their own averages is irrespective of streak.",
      style = "font-size:18px; font-family:Karla;color:black; width:75%; text-align:left;margin: 0 auto; "
    ),
    p(
      "This tool by itself suffers from the same limitations that all 
             shot charts do, in that the dots on the court offer no information 
             regarding how the shot transpired. So while it may be useful for 
             opponents to know that Klay Thompson had a +15 eFG% relative to 
             league average on all above the break 3s after 3+ Makes in 2016-17, 
             there is still more information required regarding the Warriors’ 
             sets and actions that created those attempts for Klay. One would 
             also need to know the defensive schemes that were used to defend 
             each shot. Joining the subject shot locations with the corresponding 
             film clips would be a logical extension of this analysis.",
      style = "font-size:18px; font-family:Karla;color:black; width:75%; text-align:left;margin: 0 auto; "
    ),
    br(),
    
    # Tab 2 blurb ----------------------------------------------------------------------------------------------------------------------
    
    p(
      "Tab 2: 2014-16 SportVu Closest Defender & Streaks",
      style = "font-size:24px; font-family:Georgia;color:black; width:75%; text-align:left;margin: 0 auto; font-weight: bold"
    ),
    br(),
    p(
      "The second tab of this application summarizes my results of joining 
             the 2015-16 SportVU Game Logs with shot location data. What made 
             the player tracking data so interesting for my Hot Hand analysis was 
             how I could add the nuance of closest defender distance into the 
             calculations. This tweak boils the Hot Hand down to its basic parts, 
             and helps answer the question: “Does Player X have a higher chance 
             of making more difficult shots after a string of makes?”", 
      style = "font-size:18px; font-family:Karla;color:black; width:75%; text-align:left;margin: 0 auto; "
    ),
    br(),
    p(
      "Early in the research, I noticed the high frequency of discrepancies 
             between the SportVU logs and the shot location data. This, 
             the data’s lack of information regarding when exactly each shot 
             occurred, as well as random instances of game clock discrepancies 
             of up to one minute, presented interesting challenges. The correct 
             shot time was crucial for my investigation, since it is the only 
             way to calculate closest defender distance from the raw tracking 
             data. The times in the shot location dataset unfortunately originate 
             from play-by-play data, which typically lists an event’s time 
             approximately 2 or 3 seconds after a shot occurs. In order to 
             find shot times, I created a proxy. My proxy was built from the 
             distance between the ball and the shooter, along with the play-by-play 
             event time. For each shot, I took the latest time 
             (minimum on game clock) where the Euclidean distance between the ball 
             and the shooter was less than 1.5, and the game clock was still 
             prior to the play-by-play event time. This formula yielded fairly 
             accurate results upon cross-validating my generated shot times 
             with Synergy game film. But some of the SportVU data was particularly 
             messy, with game clock columns bordering on unsalvageable. A characteristic 
             of these messy logs was a large difference between the actual amount of 
             shots taken in the game (amount of shots reflected in the shot 
             location data) and the amount of shots generated by my proxy. To 
             account for this, I created a “quality” threshold where my program 
             only accepted shots from games where this difference was less than 30. 
             One of my principles for this project was quality over quantity, since 
             I would rather have 10,000 shots with accurate closest defender 
             distances than 100,000 shots with inaccurate defender distances.",
      style = "font-size:18px; font-family:Karla;color:black; width:75%; text-align:left;margin: 0 auto; "
    ),
    p(
      "Much has been said about the questionable accuracy of tracking defender 
             distances in general, even with the Second Spectrum data used 
             by the NBA today. Defender wingspan and late contests can 
             make one “Open” a lot more contested than another. Additionally, 
             different arenas have been known to offer more or less variation 
             in their tracking cameras, which further muddies the insights 
             that can be taken away. My program ended up recording 46,405 shots 
             from the 636 available 
             game logs. Even if my method recorded every single shot perfectly 
             from those 636 games, the sample sizes here are still quite small. The 
             game logs range from the opening game of the 2015-16 season, to 
             January 23, 2016 – half a season’s worth of data, at best. In an 
             attempt to make the bins a little bigger, I condensed the commonplace 
             coverage bins of Wide Open, Open, Tight, and Very Tight to Open and 
             Tight. One silver lining is that many players have enough total FGAs 
             in this dataset to reach the “stabilization” point of FG%, which 
             is approximately ~102 attempts based on Kostya Medvedovsky’s work. 
             Another caveat I will mention here is not all “Tight” attempts are 
             created equal. A Tight attempt from 24 feet is a much 
             tougher shot than a Tight attempt from 4 feet away. However 
             the samples would be even smaller if I filtered out attempts from a 
             certain threshold of shot distance, so I decided against this.",
      style = "font-size:18px; font-family:Karla;color:black; width:75%; text-align:left;margin: 0 auto; "
    ),
    br(),
    p(
      strong("UPDATE 08/03/21:"), 
      "I added 128,069 shots from the 2014-15 NBA Season into the second tab's database. The data came courtsey of", 
      a("Dan Becker.", href="https://www.kaggle.com/dansbecker/nba-shot-logs", target="_blank"),
      style = "font-size:18px; font-family:Karla;color:black; width:75%; text-align:left;margin: 0 auto; "
    ),
    br(),
    # Acknowledgements blurb -----------------------------------------------------------------------------------------------------------
    
    p(
      "Acknowledgements",
      style = "font-size:24px; font-family:Georgia;color:black; width:75%; text-align:left;margin: 0 auto; font-weight: bold"
    ),
    br(),
    p(
      "The SportVU game logs I used are courtesy of", 
      a("Neil Seward", href="https://github.com/sealneaward", target="_blank"), 
      "at his GitHub. Neil also provided some great framework for cleaning and extracting the game logs that 
      I used in my analysis Shot location data came way of", 
      a("Alex Bresler", href="https://github.com/abresler", target="_blank"), 
      "and nbastatR. As I mentioned above  Dan Becker uploaded an amazing dataset from the 2014-15 Season with 
      closest defender distances on", 
      a("Kaggle.", href="https://www.kaggle.com/dansbecker/nba-shot-logs", target="_blank"),
      style = "font-size:18px; font-family:Karla;color:black; width:75%; text-align:left;margin: 0 auto; "
    ),
    br(),
    p(
      "I drew a lot of inspiration from various titans in the analytics space. I’ve already mentioned", 
      a("Kostya", href="https://kmedved.com/2020/08/06/nba-stabilization-rates-and-the-padding-approach/", target="_blank"), 
      "and his work about padding and stabilization. The format from Kostya and Andrew Patton’s current", 
      a("DARKO",href = "https://apanalytics.shinyapps.io/DARKO/", target="_blank"),
      "player projections influenced how I built my table in the second tab.",
      a("Owen Phillips", href = "https://thef5.substack.com/people/479475-owen-phillips", target="_blank"), 
      "at his Substack has a great tutorial on working with hexbin data with R and using it for shot 
      charts, and great R tutorials in general. Finally, thank you to the groundbreakers of Gilovich and company,
      who took the first stab at investigating the Hot Hand phenomenon.",
      style = "font-size:18px; font-family:Karla;color:black; width:75%; text-align:left;margin: 0 auto; "
    ),
    br()
  )
  
}

# -------------- #
# --- server --- #
# -------------- #

home_server <- function(id){
  
  moduleServer(id, function(input, output, session){})
  
}