-- Tables with some contrains and foreign keys
CREATE TABLE `sites` (
  `site` INTEGER UNIQUE NOT NULL,
  `site_name` TEXT,
  `latitude` NUMERIC,
  `longitude` NUMERIC,
  `elevation` NUMERIC,
  `gradient` NUMERIC,
  `region` TEXT,
  `region_general` TEXT, 
  `country` TEXT,
  `road_name` TEXT,
  `road_reference` TEXT,
  `lanes` INTEGER,
  `one_way` INTEGER,
  `direction_bound` TEXT,
  `closest_junction_distance` INTEGER,
  `closest_junction_name` INTEGER,
  `surface` TEXT,
  `footpath` TEXT,
  `cycleway` TEXT,
  `max_speed` INTEGER,
  `max_speed_unit` TEXT,
  `road_type` TEXT,
  `session_count` INTEGER,
  `open_street_map_way_id` NUMERIC,
  `notes` TEXT
);

CREATE TABLE `instruments` (
  `instrument` INTEGER UNIQUE NOT NULL,
  `instrument_manufacturer` TEXT,
  `instrument_model` TEXT,
  `instrument_name` TEXT,
  `serial_number` TEXT,
  `website` TEXT,
  `notes` TEXT
);

CREATE TABLE `vehicle_details_data_sources` (
  `data_source` INTEGER UNIQUE NOT NULL,
  `data_source_name` TEXT, 
  `data_service` TEXT,
  `jurisdiction` TEXT,
  `notes` TEXT
); 

CREATE TABLE `vehicle_captures_data_sources` (
  `data_source` INTEGER UNIQUE NOT NULL,
  `data_source_name` TEXT, 
  `notes` TEXT
);

CREATE TABLE `sites_meteorological` (
  `site` TEXT UNIQUE NOT NULL,
  `site_name` TEXT, 
  `latitude` NUMERIC,
  `longitude`	NUMERIC, 
  `elevation`	NUMERIC,
  `country`	TEXT,
  `date_start` INTEGER,
  `date_end` INTEGER,
  `data_source`	TEXT,
  `notes` TEXT
);

CREATE TABLE `field_campaigns` (
  `field_campaign` INTEGER UNIQUE NOT NULL,
  `field_campaign_name` TEXT,
  `notes` TEXT
);

CREATE TABLE `sessions` (
  `session` INTEGER UNIQUE NOT NULL,
  `site` INTEGER REFERENCES sites(site),
  `instrument` INTEGER REFERENCES instruments(instrument),
  `day` NUMERIC,
  `date_start` NUMERIC,
  `date_end` NUMERIC,
  `weather_description` TEXT,
  `company` TEXT,
  `personal` TEXT,
  `vehicle_details_data_source` INTEGER REFERENCES vehicle_details_data_sources(data_source),
  `capture_count` INTEGER,
  `registration_count` INTEGER,
  `site_met` TEXT REFERENCES sites_meteorological(site),
  `field_campaign` INTEGER REFERENCES field_campaigns(field_campaign),
  `notes` TEXT
);

CREATE TABLE `vehicle_captures` (
  `data_source` INTEGER REFERENCES vehicle_captures_data_sources(data_source),
  `session` INTEGER,
  `date` NUMERIC NOT NULL,
  `registration` TEXT,
  `variable` TEXT NOT NULL,
  `validity` INTEGER,
  `value` NUMERIC
);

CREATE TABLE `vehicle_details` (
  `data_source` INTEGER REFERENCES vehicle_details_data_sources(data_source),
  `registration` TEXT NOT NULL,
  `variable` TEXT NOT NULL,
  `value` TEXT NOT NULL
);

CREATE TABLE `vehicle_odometers` (
  `data_source` INTEGER REFERENCES vehicle_details_data_sources(data_source),
  `registration` TEXT, 
  `date` INTEGER,
  `value` NUMERIC NOT NULL,
  `unit` TEXT
);
