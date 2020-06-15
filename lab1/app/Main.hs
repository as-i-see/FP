module Main where

import           Control.Exception
import           Database.HDBC
import           Database.HDBC.PostgreSQL (connectPostgreSQL)

import           Application
import           Student
import           Teacher

main = do
  c <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=132123"
  putStrLn " -- TEACHERS -- "
  --
  allTeachers <- readAllTeachers c
  print allTeachers
  newTeacherId <- createTeacher "Boris" "Elcin" c
  print newTeacherId
  newTeacher <- readTeacher c newTeacherId
  print newTeacher
  updateTeacher newTeacherId "Vladimir" "Putin" c
  updatedTeacher <- readTeacher c newTeacherId
  print updatedTeacher
  successfullyDeletedTeacher <- deleteTeacher newTeacherId c
  print successfullyDeletedTeacher
  emptyTeacher <- readTeacher c newTeacherId
  print emptyTeacher
  -- create teacher
  newTeacherId <- createTeacher "Steve" "Jobs" c
  ---
  putStrLn " -- APPLICATIONS -- "
  ---
  allApplications <- readAllApplications c
  print allApplications
  newApplicationId <- createApplication "Musically" newTeacherId c
  print newApplicationId
  newApplication <- readApplication c newApplicationId
  print newApplication
  updateApplication newApplicationId "TikTok" newTeacherId c
  updatedApplication <- readApplication c newApplicationId
  print updatedApplication
  successfullyDeletedApplication <- deleteApplication newApplicationId c
  print successfullyDeletedApplication
  emptyApplication <- readApplication c newApplicationId
  print emptyApplication
  -- create section
  newSectionId <- createApplication "Tiktok" newTeacherId c
  --
  putStrLn " -- STUDENTS -- "
  --
  allStudents <- readAllStudents c
  print allStudents
  newStudentId <- createStudent "Maxim" "Mixovich" newSectionId c
  print newStudentId
  newStudent <- readStudent c newStudentId
  print newStudent
  updateStudent newStudentId "Maxik" "Veremchuk" newSectionId c
  updatedStudent <- readStudent c newStudentId
  print updatedStudent
  successfullyDeletedStudent <- deleteStudent newStudentId c
  print successfullyDeletedStudent
  emptyStudent <- readStudent c newStudentId
  print emptyStudent
