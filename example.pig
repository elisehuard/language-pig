users = LOAD 'sorted_log/user_registration/$date/*' USING LogStorage() AS (date:chararray, time:chararray, user_id:long);

users = FOREACH users GENERATE *, ((user_id % 100) / 10) AS cohort;
users = FOREACH users GENERATE *, (cohort <= 4 ? '04' : '59') AS herd;


active_users = LOAD 'warehouse/active_users/daily/point/{$visit_dates}*' USING ColumnStorage(' ') AS (date:chararray, user_id:long);

active_users = JOIN users BY user_id, active_users BY user_id;
active_users = FOREACH active_users GENERATE active_users::date AS date, active_users::user_id AS user_id, users::herd AS herd;


visits = GROUP active_users BY herd;
visits = FOREACH visits GENERATE group AS herd, COUNT(active_users) AS visits;

DESCRIBE visits;

report = GROUP active_users BY (date, herd);

report = FOREACH report GENERATE FLATTEN(group) AS (date, herd), COUNT(active_users) AS day_visits;
DESCRIBE report;


report = JOIN report BY herd, visits BY herd;
report = FOREACH report GENERATE report::date AS date, report::herd AS herd, report::day_visits AS day_visits, visits::visits AS visits;


define RESOLVE `python delta.py $date` SHIP('delta.py');

report = STREAM report THROUGH RESOLVE AS (day:chararray, herd:chararray, day_visits:int, visits:int);

report = FOREACH report GENERATE '$date' AS date, *;

STORE report INTO '$output' USING ColumnStorage(',');
