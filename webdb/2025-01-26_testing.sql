-- Testing
show databases;
use CDONOV12_sm_repo;
show tables;
create table test_table (
  id int primary key,
  name varchar(255)
)

describe test_table;

insert into test_table (id, name) values (1, 'fred');
insert into test_table (id, name) values (2, 'bob');
insert into test_table (id, name) values (3, 'jeff');

describe test_table;

select * from test_table;

-- experiment with metrics
use CDONOV12_sm_repo;
drop table metrics;
show tables;
create table metrics (
  fips varchar(5),
  year int,
  variable_name varchar(255),
  value double,
  primary key (fips, year, variable_name)
)

describe metrics;

-- upload csv
select @@secure_file_priv;
-- this is where we can store a file to upload
-- but we can't fucking edit this, or make a real my.ini file??
