USE LosAngelesCrimes

DROP TABLE CrimesData

SELECT LA_crimes.Area, Neighborhood, LA_Crimes,
		Total_Population,
		[Under_Age_18], [Ages_18-24], [Ages_25-34], [Ages_35-44], [Ages_45-54], [Ages_55-64], [Ages_65_&_Older],
		Young_Adults_Enrolled_in_School,
		College_Graduation_Rate, Less_Than_High_School,
		Labor_Force_Participation_Rate, Unemployment_Rate,
		Homeownership_Rate, Renter_Rate,
		[Immigrant_Citizen], [Immigrant_Non-Citizen], [Immigrant],
		Divorced_Separated, Married, Never_Married, Widowed
		Median_Household_Income,
		Opportunity_Youth,
		Homeless_count,
		Below_100_Poverty_Threshold, Below_200_Poverty_Threshold,
		White, American_Indian_Or_Native, Asian, Black, Hispanic, Other_Race, Two_or_More_Races,
		Median_Rent_Price,
		No_Vehicle_Households,
		[Youth_in_Non-Family_Households], [Youth_in_Single_Parent_Households]
INTO CrimesData
FROM LA_crimes
LEFT JOIN Neighborhoods ON LA_crimes.Area = Neighborhoods.Area
LEFT JOIN Total_Population ON LA_crimes.Area = Total_Population.Area
LEFT JOIN Age_Distribution ON LA_crimes.Area = Age_Distribution.Area
LEFT JOIN College_Enrollment ON LA_crimes.Area = College_Enrollment.Area
LEFT JOIN Educational_Attainment ON LA_crimes.Area = Educational_Attainment.Area
LEFT JOIN Employment_Status ON LA_crimes.Area = Employment_Status.Area
LEFT JOIN Homeownership ON LA_crimes.Area = Homeownership.Area
LEFT JOIN Immigration ON LA_crimes.Area = Immigration.Area
LEFT JOIN Marital_Status ON LA_crimes.Area = Marital_Status.Area
LEFT JOIN Median_Household_Income ON LA_crimes.Area = Median_Household_Income.Area
LEFT JOIN Opportunity_Youth ON LA_crimes.Area = Opportunity_Youth.Area
LEFT JOIN Poverty ON LA_crimes.Area = Poverty.Area
LEFT JOIN Homelessness ON LA_crimes.Area = Homelessness.Area
LEFT JOIN Race ON LA_crimes.Area = Race.Area
LEFT JOIN Rent ON LA_crimes.Area = Rent.Area
LEFT JOIN Vehicles ON LA_crimes.Area = Vehicles.Area
LEFT JOIN Youth ON LA_crimes.Area = Youth.Area

SELECT * FROM CrimesData

