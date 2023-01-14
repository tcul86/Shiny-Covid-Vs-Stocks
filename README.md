# COVID-19 vs Stocks: Making an Interactive Dashboard for High School Learning

**Authors: Toby Cullen, Link Ding, Thomas Elton, Kosta Konstant, Josh Sung, Sarah Sweeting.**

This is the authors major group project for DATA3888 (Data Science Capstone) at the University of Sydney. We were tasked with creating a Shiny App on
whatever topic we pleased, as long as it connected to the [COVID-19 data set from John Hopkins University](https://github.com/CSSEGISandData/COVID-19).
Resultingly, we decided to create an interactive classroom experience, addressing the affect of COVID-19 on Stocks. Our Shiny App is desgined for students
to be able to experience self-discovery through interactive plots, whilst they are guided with discussion questions.

## Shiny App Link

The Shiny App code can be found in the 'Shiny-App' folder.

The Shiny App can be accessed [here](https://thomaselton.shinyapps.io/finance-during-covid-shiny/). Please be aware that we are on the free version of
'Shinyapps.io', and as such, all of the publishers Shiny Apps combined cannot exceed 25 active hours a month.

## Report Excerpts

### Introduction

In the last few decades, pedagogy has seen a shift as traditional didactic lecture-approaches have been seen as ineffective as opposed to active learning 
models of teaching (Jungst, Wiersema & Licklider, 2003). Active learning is a teaching strategy where the student plays an energetic and dynamic role in 
their own education, where they are not a passive participant in the classroom, rather, they actively engage in classroom activities (Petress, 2008; 
Felder & Brent, 2009). Active learning has been shown to have positive outcomes on student learning, as well as stimulating critical thinking and positive
attitudes towards class (Gorres-Martens, Segovia & Pfefer, 2016; Bokosmaty, Bridgeman, Muir, 2019).

Keeping this literature in mind, we created an interactive dashboard lesson to aid New South Wales commerce and economic teachers in delivering an engaging
lesson relating to the effect of Covid-19 on equity prices. Students are prompted with discussion questions, but ultimately, students drive their own 
educational experience via actively engaging with dynamic graphs and interactive features. The lesson objectives for the dashboard were made to align 
with key syllabus dot points.

Additionally, in designing the dashboard, key attention was paid to cognitive load theory (CLT). CLT is an instructional design theory which accounts for 
how only small amounts of new information can be processed in short-term memory at any given time (Sweller, 2011). This is due to the limited capacity of 
short-term memory (Miller, 1956; Hulme et. al., 1995; Service, 1998), and how if this limited capacity is exceeded, new information is not successfully 
encoding into one’s long-term memory (Martin, 2016). Information not being stored into long-term memory clearly represents a failure in pedagogy, as one 
of the goals of teaching is for students to retain what they have learned. This led to the dashboard design described in the next section of this report.

### Methods - Data Sources

The dashboard requires data to analyse, and as such, two main data sources were acquired. The first was COVID-19 data set on GitHub from John Hopkins 
University. This data set aggregates data from multiple international sources, allowing for an international timeseries of COVID statistics 
(statistics such as cumulative covid cases, cumulative deaths, vaccination rate, etc.).

The second main data source was timeseries stock prices which was acquired through the use of application programming interfaces (APIs). The dashboard
utilised two APIs, the ASX company directory API, and the Yahoo Finance API.

### Methods - Creating the Dashboard

The dashboard was designed primarily utilising the Shiny and Shiny Dashboard R packages. In accordance with CLT, as well as ensuring that the active
learning roots of the dashboard were maintained, careful design choices were adhered to. As such, questions were designed with self-discovery in mind. 
This is evident in Figure 1.

<div id="header" align="center">
  <img src="https://github.com/tjelton/Shiny-Covid-Vs-Stocks/blob/main/Images/image_1.jpeg"/>
</div>

***Figure 1: Example question in the dashboard where students are asked guiding questions (in blue), and are then free to explore the plot on the 
right to inform their answer. The code to produce these plots in the shiny is given in the code chunk above.***

As evident from the figure, students are given guiding discussion questions, which are always highlighted in blue for consistency. Students are then free
to formulate answers in response to these discussion questions, by toggling buttons (see the “Add Milestones to Graph” in the left of Figure 1) and
observing the effect to the graph on the right of the information text box. Additionally, students can interact with the graph by zooming in, or even
just hovering of the lines, such as how in Figure 1, the mouse is hovering on the blue line, causing an informative blue text box to appear.

Additionally, to reduce cognitive load, each module in the shiny is separated by a blue continue button, which a student must click to proceed to the next
section (as seen in figure 1). The theoretical basis of this is that it limits the visual stimuli on the scream, helping student to focus on the content
in front of them. This aims to reduce the volume on short-term memory and should theoretically aid students encoding newfound content into long-term memory.

Another consideration regarding CLT was for modules to build upon each other. This was designed in such a way so that students could become familiar with
simpler examples, before moving on to more complex ones. This is evident in Figure 2, which indicates how a module where students observe the trend in
COVID cases in Australia vs the BHP stock price, is followed by a module where students are presented with the same graph, yet this time they can alter
the stock and COVID statistic being used. This allows students to comprehend the simpler visualisation, before moving on to the more complex one.

<div id="header" align="center">
  <img src="https://github.com/tjelton/Shiny-Covid-Vs-Stocks/blob/main/Images/image_2.jpeg"/>
</div>

***Figure 2: The top module is a more restricted version of the bottom module, meaning that students have the opportunity to master the simpler version,
before moving on to the more complex one.***

### References

> For references of the R packages, API's and data sets used, please see the "References" page of the Shiny app.

Bokosmaty, R., Bridgeman, A., Muir, M. (2019). Using a Partially Flipped Learning Model To Teach First Year Undergraduate Chemistry. Journal of Chemical
Education, 96(4), 629-639. https://doi.org/10.1021/acs.jchemed.8b00414

Felder, R. M., Brent, R. (2009). Active Learning: An Introduction. ASQ Higher Education Brief. https://www.engr.ncsu.edu/wp-content/uploads/drive/1YB2KK3wLqP3EhXyYdKtE9-4mBJzc2rc2/Active%20Learning%20Tutorial.pdf

Gorres-Martens, B. K., Segovia, A. R., Pfefer, M. T. (2016). Positive outcomes increase over time with the implementation of a semiflipped teaching model.
Advanced in Physiology Education. https://doi.org/10.1152/advan.00034.2015

Hulme, C., Roodenrys, S., Brown, G., Mercer, R. (1995). The role of long-term memory mechanisms in memory span. British Journal of Psychology, 86(4), 
527-536. https://doi.org/10.1111/j.2044- 8295.1995.tb02570.x

Miller, G. A. (1956). The Magical Number Seven, Plus or Minus Two: Some Limits on our Capacity for Processing Information. Psychological Review, 63(2),
81-97. https://doi.org/10.1037/h0043158

Jungst, S., Wiersema, J., Licklider, B. (2003). Providing Support for Faculty Who Wish to Shift to a Learning- Centered Paradigm in Their Higher Education
Classrooms. Journal of the Scholarship of Teaching and Learning, 3(3), 69-81. 
https://www.proquest.com/docview/2387867986?pq-origsite=gscholar&fromopenview=true

Petress, K. (2008). What is Meant by “Active Learning?”. Education, 128(4), 566-569. https://go.gale.com/ps/i.do?id=GALE%7CA303642817&sid=googleScholar&v=2.1&it=r&linkaccess=abs&issn=00131172&p=AONE&sw=w&userGroupName=usyd

Service, E. (1998). The Effect of Word Length on Immediate Serial Recall Depends on Phonological Complexity, Not Articulatory Duration. 
Quarterly Journal of Experimental Psychology, 51(2), 283-304. https://doi.org/10.1080/713755759

Sweller, J. (2011). CHAPTER TWO – Cognitive Load Theory. In J. P. Mestre & B. H. Ross, Psychology of Learning and Motivation (pp. 37-76). 
Elsevier.
