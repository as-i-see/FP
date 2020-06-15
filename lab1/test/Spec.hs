import           Common
import           Application
import           Student
import           Teacher

import           Test.Tasty               (defaultMain, testGroup)
import           Test.Tasty.HUnit         (assertEqual, assertFailure, testCase)

import           Database.HDBC
import           Database.HDBC.PostgreSQL (connectPostgreSQL)

main = defaultMain allTests

allTests = testGroup "All tests" [teacherTests, applicationTests, studentTests]

teacherTests = testGroup "Teacher tests" [createTeacherTest, updateTeacherTest]

applicationTests = testGroup "Application tests" [createApplicationTest, updateApplicationTest]

studentTests = testGroup "Student tests" [createStudentTest, updateStudentTest]

createTeacherTest =
  testCase "Create teacher" $ do
    c <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=132123"
    newTeacherId <- createTeacher "New" "Teacher" c
    teacher <- readTeacher c newTeacherId
    assertEqual "Teacher ID equal" newTeacherId (Common.getTeacherId teacher)
    assertEqual "Teacher Name equal" "New" (Common.getTeacherName teacher)
    assertEqual "Teacher Surname equal" "Teacher" (Common.getTeacherSurname teacher)
    removed <- deleteTeacher newTeacherId c
    assertEqual "Teacher removed" True removed

updateTeacherTest =
  testCase "Update teacher" $ do
    c <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=132123"
    newTeacherId <- createTeacher "New" "Teacher" c
    teacher <- readTeacher c newTeacherId
    assertEqual "Teacher ID equal" newTeacherId (Common.getTeacherId teacher)
    assertEqual "Teacher Name equal" "New" (Common.getTeacherName teacher)
    assertEqual "Teacher Surname equal" "Teacher" (Common.getTeacherSurname teacher)
    teacher <- updateTeacher newTeacherId "Newest" "Coach" c
    assertEqual "Teacher ID equal" newTeacherId (Common.getTeacherId teacher)
    assertEqual "Teacher Name equal" "Newest" (Common.getTeacherName teacher)
    assertEqual "Teacher Surname equal" "Coach" (Common.getTeacherSurname teacher)
    removed <- deleteTeacher newTeacherId c
    assertEqual "Teacher removed" True removed

createApplicationTest =
  testCase "Create application" $ do
    c <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=132123"
    newTeacherId <- createTeacher "New" "Teacher" c
    newApplicationId <- createApplication "KWAI" newTeacherId c
    application <- readApplication c newApplicationId
    assertEqual "Application ID equal" newApplicationId (Common.getApplicationId application)
    assertEqual "Application Name equal" "KWAI" (Common.getApplicationName application)
    assertEqual "Application TeacherID equal" newTeacherId (Common.getApplicationTeacherId application)
    removed <- deleteApplication newApplicationId c
    assertEqual "Application removed" True removed
    removed <- deleteTeacher newTeacherId c
    assertEqual "Application removed" True removed

updateApplicationTest =
  testCase "Update application" $ do
    c <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=132123"
    newTeacherId <- createTeacher "New" "Teacher" c
    newApplicationId <- createApplication "KWAI" newTeacherId c
    application <- readApplication c newApplicationId
    assertEqual "Application ID equal" newApplicationId (Common.getApplicationId application)
    assertEqual "Application Name equal" "KWAI" (Common.getApplicationName application)
    assertEqual "Application TeacherID equal" newTeacherId (Common.getApplicationTeacherId application)
    application <- updateApplication newApplicationId "Insta" newTeacherId c
    assertEqual "Application ID equal" newApplicationId (Common.getApplicationId application)
    assertEqual "Application Name equal" "Insta" (Common.getApplicationName application)
    assertEqual "Application TeacherID equal" newTeacherId (Common.getApplicationTeacherId application)
    removed <- deleteApplication newApplicationId c
    assertEqual "Application removed" True removed
    removed <- deleteTeacher newTeacherId c
    assertEqual "Teacher removed" True removed

createStudentTest =
  testCase "Create student" $ do
    c <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=132123"
    newTeacherId <- createTeacher "New" "Teacher" c
    newApplicationId <- createApplication "KWAI" newTeacherId c
    newStudentId <- createStudent "Bambito" "Liderito" newApplicationId c
    student <- readStudent c newStudentId
    assertEqual "Student ID equal" newStudentId (Common.getStudentId student)
    assertEqual "Student Name equal" "Bambito" (Common.getStudentName student)
    assertEqual "Student Surname equal" "Liderito" (Common.getStudentSurname student)
    assertEqual "Student ApplicationID equal" newApplicationId (Common.getStudentApplication student)
    removed <- deleteStudent newStudentId c
    assertEqual "Student removed" True removed
    removed <- deleteApplication newApplicationId c
    assertEqual "Application removed" True removed
    removed <- deleteTeacher newTeacherId c
    assertEqual "Teacher removed" True removed

updateStudentTest =
  testCase "Update student" $ do
    c <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=132123"
    newTeacherId <- createTeacher "New" "Teacher" c
    newApplicationId <- createApplication "KWAI" newTeacherId c
    newStudentId <- createStudent "Bambito" "Liderito" newApplicationId c
    student <- readStudent c newStudentId
    assertEqual "Student ID equal" newStudentId (Common.getStudentId student)
    assertEqual "Student Name equal" "Bambito" (Common.getStudentName student)
    assertEqual "Student Surname equal" "Liderito" (Common.getStudentSurname student)
    assertEqual "Student ApplicationID equal" newApplicationId (Common.getStudentApplication student)
    student <- updateStudent newStudentId "Lolipop" "Kekovich" newApplicationId c
    assertEqual "Student ID equal" newStudentId (Common.getStudentId student)
    assertEqual "Student Name equal" "Lolipop" (Common.getStudentName student)
    assertEqual "Student Surname equal" "Kekovich" (Common.getStudentSurname student)
    assertEqual "Student ApplicationID equal" newApplicationId (Common.getStudentApplication student)
    removed <- deleteStudent newStudentId c
    assertEqual "Student removed" True removed
    removed <- deleteApplication newApplicationId c
    assertEqual "Application removed" True removed
    removed <- deleteTeacher newTeacherId c
    assertEqual "Teacher removed" True removed
