# COVID-19 vs Stocks: Making an Interactive Dashboard for High School Learning

**Authors: Toby Cullen, Link Ding, Thomas Elton, Kosta Konstant, Josh Sung, Sarah Sweeting.**

This is the authors major group project for DATA3888 (Data Science Capstone) at the University of Sydney. We were tasked with creating a Shiny App on
whatever topic we pleased, as long as it connected to the [COVID-19 data set from John Hopkins University](https://github.com/CSSEGISandData/COVID-19).
Resultingly, we decided to create an interactive classroom experience, addressing the affect of COVID-19 on Stocks. Our Shiny App is desgined for students
to be able to experience self-discovery through interactive plots, whilst they are guided with discussion questions.

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
