########R Intro - Final Exercise########

library(DBI)
library(dplyr)

con <- dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "localhost\\SQLEXPRESS", 
                      Database = "COLLEGE", 
                      Trusted_Connection = "True")
conn=con

Classrooms<- dbGetQuery(con, 'SELECT * FROM "COLLEGE"."dbo"."Classrooms"')
Courses<- dbGetQuery(con, 'SELECT * FROM "COLLEGE"."dbo"."Courses"')
Departments<- dbGetQuery(con, 'SELECT * FROM "COLLEGE"."dbo"."Departments"')
Students<- dbGetQuery(con, 'SELECT * FROM "COLLEGE"."dbo"."Students"')
Teachers<- dbGetQuery(con, 'SELECT * FROM "COLLEGE"."dbo"."Teachers"')

### In windows, Using a ODBC DNS (predefined connection name)
### Some possible strings for the driver:
### the DSN must be the same as you created in the ODBC (check it!)
driver <- "Driver={SQL Server};DSN=COLLEGE;Trusted_Connection=yes;"

driver <- "Driver={SQL Server Native Connection 11.0};DSN=COLLEGE;Trusted_Connection=True;"

### XXXXX\\XXXXX is the name of the server as it appears in the SQL server management studio
### COLLEGE is the name of the database (check how do you called it in your local server)
driver <- "Driver={SQL Server Native Connection 11.0};Server=XXXXX\\XXXXX;Database=COLLEGE;Trusted_Connection=True;"


### Try with the diferent driver strings to see what works for you
conn <- dbConnect(odbc::odbc, .connection_string = driver)


### Get the students table
students = dbQuery(conn, "SELECT * FROM Students")

dbDisconnect(conn)


## Change DepaertmentID column name in courses to match the other tables:
Courses <- Courses %>% 
  rename(DepartmentId=DepartmentID)


## Questions
## Q1. Count the number of students on each department
# Join tables of classrooms, courses and departments:
class_cour <- left_join(Classrooms, Courses, by="CourseId")
dep_class_cour <- left_join(Departments, class_cour, by="DepartmentId")
# Remove duplicates and group by department:
dep_class_cour %>%
  distinct(StudentId, DepartmentId, .keep_all = TRUE) %>%
  group_by(DepartmentName) %>%
  summarise (number_of_students=n())


##Q2. How many students have each course of the English department and the total number of students in the department?
# Join tables of classrooms, courses and departments:
class_cour <- left_join(Classrooms, Courses, by="CourseId")
dep_class_cour <- left_join(Departments, class_cour, by="DepartmentId")
# Remove duplicates and find all courses in English dep and count students: 
dep_class_cour %>%
  distinct(StudentId, CourseId, .keep_all = TRUE) %>%
  group_by(CourseName) %>%
  filter(DepartmentName=='English') %>%
  summarise (number_of_students=n()) %>%
  add_row(CourseName='total',number_of_students=as.integer(dep_class_cour %>%
                                                             distinct(StudentId, DepartmentId, .keep_all = TRUE) %>%
                                                             filter(DepartmentName=="English") %>%
                                                             summarise(n())))
          

## Q3. How many small (<22 students) and large (22+ students) classrooms are needed for the Science department?
# Join tables of classrooms, courses and departments:
class_cour <- left_join(Classrooms, Courses, by="CourseId")
dep_class_cour <- left_join(Departments, class_cour, by="DepartmentId")
# Group the number of students per class and identify big and small classes:
dep_class_cour %>%
  distinct(StudentId, CourseId, .keep_all = TRUE) %>%
  group_by(CourseName) %>%
  filter(DepartmentName=='Science') %>%
  summarise (number_of_students=n()) %>%
  mutate (Class_size = ifelse (number_of_students<22, 'Small classrooms', 'Big classrooms')) %>% 
  group_by(Class_size) %>%
  summarise (number_of_classes=n())


## Q4. A feminist student claims that there are more male than female in the College. Justify if the argument is correct
# Group by gender and count the nambers of men and women among all students:
Students %>%
  group_by(Gender) %>%
  summarise (number_of_students=n())


## Q5. For which courses the percentage of male/female students is over 70%?
# Join tables of classrooms, courses and students:
class_cour <- left_join(Classrooms, Courses, by="CourseId")
stu_class_cour <- left_join(Students, class_cour, by="StudentId")
# Create a table to show number of women in each course:
Females <- stu_class_cour %>%
  filter(Gender=='F') %>%
  group_by(CourseId, CourseName) %>%
  summarise (Women=n())
# Create a table to show number of men in each course:
Males <- stu_class_cour %>%
  filter(Gender=='M') %>%
  group_by(CourseName) %>%
  summarise (Men=n())
# Merge the two tables to the result, showing number of men, women and ratio
# and present courses with ratio over 70:
inner_join(Females, Males, by="CourseName") %>%
  mutate (Ratio = Women/(Women+Men)*100) %>%
  filter(Ratio>70)


### Q6. For each department, how many students passed with a grades over 80?
# Join tables of classrooms, courses and departments:  
class_cour <- left_join(Classrooms, Courses, by="CourseId")
dep_class_cour <- left_join(Departments, class_cour, by="DepartmentId")
# Create a table to show number of students in each department:  
All_students <- dep_class_cour %>%
  distinct(StudentId, DepartmentName, .keep_all = TRUE) %>%
  group_by(DepartmentName) %>%
  summarise (number_of_all_students=n())
# Create a table to show number of top students in each department (grade over 80) and group by department:
Top_students <- dep_class_cour %>%
  filter(degree>80) %>%
  distinct(StudentId, DepartmentName, .keep_all = TRUE) %>%
  group_by(DepartmentName) %>%
  summarise (number_of_top_students=n())
# Merge the two tables to the result, showing number of all students, top students and ratio:
inner_join(All_students, Top_students, by="DepartmentName") %>%
  mutate (Ratio = number_of_top_students/number_of_all_students*100)


### Q7. For each department, how many students passed with a grades under 60?
# Join tables of classrooms, courses and departments:  
class_cour <- left_join(Classrooms, Courses, by="CourseId")
dep_class_cour <- left_join(Departments, class_cour, by="DepartmentId")
# Create a table to show number of students in each department:  
All_students <- dep_class_cour %>%
  distinct(StudentId, DepartmentName, .keep_all = TRUE) %>%
  group_by(DepartmentName) %>%
  summarise (number_of_all_students=n())
# Create a table to show number of low students in each department (grade over 80) and group by department:
Low_students <- dep_class_cour %>%
  filter(degree<60) %>%
  distinct(StudentId, DepartmentName, .keep_all = TRUE) %>%
  group_by(DepartmentName) %>%
  summarise (number_of_low_students=n())
# Merge the two tables to the result, showing number of all students, low students and ratio:
inner_join(All_students, Low_students, by="DepartmentName") %>%
  mutate (Ratio = number_of_low_students/number_of_all_students*100)


### Q8. Rate the teachers by their average student's grades (in descending order).
#Join tables of classrooms, courses and teachers:
class_cour <- left_join(Classrooms, Courses, by="CourseId")
tea_class_cour <- left_join(Teachers, class_cour, by="TeacherId")
# Group teachers, calculate the average degree in their corses and return in descending order:
tea_class_cour %>%
  group_by(TeacherId) %>%
  summarise (Teacher_first_name = min(FirstName,na.rm=TRUE),
             Teacher_last_name = min(LastName, na.rm=TRUE),
             Average_grade = mean(degree)) %>%
  arrange(desc(Average_grade))


### Q9. Create a dataframe showing the courses, departments they are associated with, the teacher in each course, and the number of students enrolled in the course (for each course, department and teacher show the names).
# Join tables of courses, departments, teachers and classrooms:
dep_cour <- left_join(Departments, Courses, by="DepartmentId")
tea_dep_cour <- left_join(Teachers, dep_cour, by="TeacherId")
cla_tea_dep_cour <- right_join(Classrooms, tea_dep_cour, by="CourseId")
# Group courses together:
cla_tea_dep_cour %>%
  group_by(CourseId) %>%
  summarise (Course_name = min(CourseName,na.rm=TRUE),
             Department_name = min(DepartmentName,na.rm=TRUE),
             Teacher_first_name = min(FirstName,na.rm=TRUE),
             Teacher_last_name = min(LastName, na.rm=TRUE),
             number_of_students=n())


### Q10. Create a dataframe showing the students, the number of courses they take, the average of the grades per class, and their overall average (for each student show the student name).
## First option per question:
# Join tables of students, classrooms and courses:
stu_cls <- left_join(Students, Classrooms, by="StudentId")
stu_cls_cour <- left_join(stu_cls, Courses, by="CourseId")
# Gegerate a table to calculate the number of courses each student takes:
NumCourses <- stu_cls_cour %>%
  group_by(StudentId) %>%
  summarise (number_of_courses=n())
# Generate a table to claculate the average grade in all courses
GPA <- stu_cls_cour %>%
  group_by(StudentId) %>%
  summarise (GPA=mean(degree))
# Merge all tables to show the number of courses, average per course as well as the total GPA:
a <- left_join(stu_cls_cour, NumCourses, by="StudentId")
Result <- left_join(a, GPA, by="StudentId")
Result %>% summarise (StudentId,
                      First_name=FirstName,
                      Last_name=LastName,
                      number_of_courses,
                      Course_name=CourseName,
                      Grade=degree,
                      total_GPA=GPA)

## Second option per provided answer table:
# Join tables of students, classrooms, courses and departments:
stu_cls <- left_join(Students, Classrooms, by="StudentId")
stu_cls_cour <- left_join(stu_cls, Courses, by="CourseId")
stu_cls_cour_dep <- left_join(stu_cls_cour, Departments, by="DepartmentId")
# Generate a table to calculate the number of courses each student takes:
NumCourses <- stu_cls_cour_dep %>%
  group_by(StudentId, First_Name=FirstName,Last_name=LastName) %>%
  summarise (number_of_courses=n())
# Generate a table to claculate the average grade in all courses
GPA <- stu_cls_cour_dep %>%
  group_by(StudentId) %>%
  summarise (GPA=mean(degree))
# Generate tables for each deaprtment to claculate the average grade of all courses in that department:
Arts_GPA<-stu_cls_cour_dep %>%
  filter (DepartmentName=='Arts') %>%
  group_by(StudentId) %>%
  summarise (Arts=mean(degree))
English_GPA<-stu_cls_cour_dep %>%
  filter (DepartmentName=='English') %>%
  group_by(StudentId) %>%
  summarise (English=mean(degree))
Science_GPA<-stu_cls_cour_dep %>%
  filter (DepartmentName=='Science') %>%
  group_by(StudentId) %>%
  summarise (Science=mean(degree))
Sports_GPA<-stu_cls_cour_dep %>%
  filter (DepartmentName=='Sport') %>%
  group_by(StudentId) %>%
  summarise (Sports=mean(degree))
### Join all tables to present the information about each student:
a <- left_join(NumCourses, English_GPA, on='StudentId')
b <- left_join(a, Arts_GPA, on='StudentId')
c <- left_join(b, Science_GPA, on='StudentId')
d <- left_join(c, Sports_GPA, on='StudentId')
result <- left_join(d, GPA, on='StudentId')
result

