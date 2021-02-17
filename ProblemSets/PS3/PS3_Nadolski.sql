#!/bin/sh

-- SQL code - reading in data
-- Create Table for data to live in 

CREATE TABLE FL_data(
	"policyID" INTEGER,
	"statecode" CHAR,
	"county" CHAR,
	"eq_site_limit" INTEGER,
	"hu_site_limit" INTEGER,
	"fl_site_limit" INTEGER,
	"fr_site_limit" INTEGER,
	"tiv_2011" REAL,
	"tiv_2012" REAL,
	"eq_site_deductible" REAL,
	"hu_site_deductible" REAL,
	"fl_site_deductible" REAL,
	"fr_site_deductible" REAL,
	"point_latitude" REAL,
	"point_longitude" REAL,
	"line" CHAR,
	"construction" CHAR,
	"point_granularity" INTEGER
);

-- Import data
.mode csv
.import FL_insurance_sample.csv FL_data

-- Print out first 10 rows
SELECT * FROM FL_data LIMIT 10;	

-- List unique counties in the sample
SELECT county, COUNT(*) FROM FL_data GROUP BY county;

-- Compute average property appreciation from 2011 to 2012
SELECT AVG(tiv_2012 - tiv_2011) FROM FL_data;

-- Create a frequency table of uniq construction
SELECT construction, count(policyID) FROM FL_data GROUP BY construction;
