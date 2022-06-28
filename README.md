# Econometrics Data Project

## Tools and Technologies used
- R 
- R studio
- Python

<br />

## Topic of Project
Does rainfall and percentage of toilets have an effect on Percentage of children with Measles in the age group of 0 to 5 years

<br />

## Steps followed in Project
- Model Creation : We first theoretically researched and decided the independent variables in
our model
- Conducted Model Analysis
- Researched intensively to select creative variables
- Calculated Correlations
- Found out the regression and interpreted the results
- Came up with relevant policy suggestions

<br />

## About the Project
Our model-
<img src="https://github.com/diyaahuja/Data-Assignment-Econometrics/blob/main/images/equation.jpg" align="left" style="width: 100%" />


| Variable  | Description                                                                                                              | Reason for choosing                                                                                                                                                                                                                                                                                                                                                                   |
|-----------|--------------------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| log(Gdp)  | State Wise GDP                                                                                                           | If the GDP of the state or district increases then<br>generally it is assumed(because income inequality is<br>not considered in GDP calculation) that the income of<br>the citizens of that area also increases. Consequently<br>increase in income leads to better access to<br>healthcare and basic amenities of life and hence a<br>lower chance of contracting measles.           |
| tap       | District Wise Tap Water Access<br>(Percentage of Households) as of<br>2019                                               | Measles is a highly contagious disease and hence<br>can be easily spread among many people. Still, a<br>huge part of India does not have access to safe and<br>clean drinking water, which can prove to be very<br>dangerous in case of a measles outbreak.                                                                                                                           |
| rain      | State Wise Annual Rainfall                                                                                               | t has been discovered that there has been a strong and consistent pattern of<br>measles outbreaks that have been associated with rainfall.<br>There has been an inverse relation between occurrence of Rainfall and<br>Measles.During the rainy season, number of reported Measles cases were less, however<br>they were high in the dry season.                                      |
| toilet    | Percentage of Rural Households <br>with access to toilet                                                                 | Bad sanitisation facilities such as lack of hygiene, immunisation being less,<br>overcrowding etc increases the risk of measles transmission.<br>There is an inverse relation between Sanitization Facilities and Measles.<br>When the sanitisation facilities are good, number of reported Measles cases<br>are less, however they are high when the sanitisation facilities are bad |
| log(beds) | State Wise Number of Hospital Beds<br>(as of 2020)                                                                       | In case of a measles outbreak, the number of beds in<br>hospitals will increase to accommodate the rise in<br>demand for hospital beds, similar to what had<br>happened in covid as both are highly transmissible<br>diseases.                                                                                                                                                        |
| log(v34)  | Fully immunized<br>children in the age<br>group of 9 to 11<br>months                                                     | It has been observed that immunization has caused a drastic<br>decline in measles. Fully immunized children in the age group of 9<br>to 11 months are at a much lower risk of contracting measles and<br>hence affect the total percentage of children with measles in the<br>age group pf 0 to 5 years.                                                                              |
| v37       | Percentage of children with Diarrhea<br>and Dehydration in the<br>age group of 0 to 5 years                              | Diarrhea and dehydration are a few of the symptoms of measles<br>and are generally categorized as complications associated with<br>the disease, specifically in children under the age of 5 years. Thus,<br>the percentage of children with measles is correlated with the<br>percentage of children with diarrhea and dehydration.                                                   |
| v16       | Percentage of<br>safe deliveries (to<br>total reported<br>deliveries)                                                    | An unsafe delivery typically depicts a weaker immunity in the<br>newborn and a higher chance of contracting infections such as<br>measles. The complications can also be expected to be much<br>worse in the case of a newborn with unsafe delivery.                                                                                                                                  |
| v28       | Percentage of<br>newborns having<br>weight less than<br>2.5 kg                                                           | Children born with low birth weight, that is less than 2.5 kg are at<br>a significant risk of developing diseases like measles in upcoming<br>years                                                                                                                                                                                                                                   |
| v46       | Percentage of<br>infant deaths due<br>to Measles (to total<br>reported<br>infant deaths)                                 | Measles can be fatal in people of all ages. However, complications<br>are more likely in children under the age of five and adults over<br>the age of twenty. Ear infections and diarrhea are common<br>complications. Pneumonia and encephalitis are serious<br>complications.                                                                                                       |
| v21       | Percentage of women<br>received a postpartum<br>checkup or PostNatal Care between<br>48 hours to 14 days of<br>delivery. | After birth, women and infants require assistance and close<br>supervision. Mothers usually take their newborns along with<br>them to their postpartum checkups. According to WHO, the<br>majority of mother and baby fatalities occur within the first<br>six weeks following delivery.                                                                                              |

<br />
##Data Analysis done using
- 40+ Histograms and scatter plots
- Hypothesis Testing using F-Test and T-Test
- Monto Carlo Simulations
- Measures of Central Tendancies using Mean, Media and Mode
- Correlation between variables 
- Linear Regression Analysis
