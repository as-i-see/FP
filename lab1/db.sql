CREATE TABLE teacher
(
    id      SERIAL PRIMARY KEY,
    name    VARCHAR(20) NOT NULL,
    surname VARCHAR(20) NOT NULL
);


CREATE TABLE application
(
    id        SERIAL PRIMARY KEY,
    name      VARCHAR(20) NOT NULL,
    teacherId INT REFERENCES teacher (id)
);

CREATE TABLE student
(
    id        SERIAL PRIMARY KEY,
    name      VARCHAR(20) NOT NULL,
    surname   VARCHAR(20) NOT NULL,
    applicationId INT REFERENCES application (id)
);


CREATE TABLE seminar_schedule
(
    id        SERIAL PRIMARY KEY,
    applicationId INT REFERENCES application (id) NOT NULL,
    beginDay  VARCHAR(3)                       NOT NULL,
    beginTime TIME                             NOT NULL,
    endTime   TIME                             NOT NULL
);

CREATE TABLE tests_schedule
(
    id        SERIAL PRIMARY KEY,
    applicationId SERIAL REFERENCES application (id) NOT NULL,
    begin_    TIMESTAMP                           NOT NULL,
    end_      TIMESTAMP                           NOT NULL,
    testerId    INT REFERENCES student (id)
);

INSERT INTO teacher
        (name, surname)
VALUES  ('Carl', 'Karl'),
        ('Michael', 'Jackson'),
        ('Lil', 'Peep');

INSERT INTO application
        (name, teacherID)
VALUES  ('Lun', 1),
        ('KotBayun', 2),
        ('Pechenegs', 3);

INSERT INTO student
        (name, surname, applicationId)
VALUES  ('Albert', 'Balbes', 1),
        ('Coder', 'Bober', 1),
        ('Dod', 'Bot', 2),
        ('Ersh', 'Best', 2),
        ('Gregory', 'Buffout', 3),
        ('Hugo', 'Bingo', 2),
        ('Lesly', 'Dindon', 3),
        ('Fosley', 'Back', 1);

INSERT INTO seminar_schedule
        (applicationId, beginDay, beginTime, endTime)
VALUES  (1, 'Mon', '16:00', '18:00'),
        (1, 'Trd', '16:00', '18:00'),
        (2, 'Wed', '16:00', '18:00'),
        (2, 'Frd', '17:00', '19:00'),
        (3, 'Tue', '18:00', '21:00'),
        (3, 'Sun', '19:00', '21:00');

INSERT INTO tests_schedule
        (applicationId, begin_, end_)
VALUES  (1, '2016-11-04 14:00:00', '2016-11-04 20:00:00'),
        (2, '2016-11-05 16:00:00', '2016-11-05 20:00:00'),
        (1, '2016-11-07 12:00:00', '2016-11-07 20:00:00');