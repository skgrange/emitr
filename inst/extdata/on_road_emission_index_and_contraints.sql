-- Indices
CREATE INDEX vehicle_captures_registration_index
  ON vehicle_captures(registration);

CREATE INDEX vehicle_details_registration_index
  ON vehicle_details(registration);

CREATE INDEX vehicle_details_data_source_index 
  ON vehicle_details(data_source);

-- DROP INDEX vehicle_captures_registration_index;
-- DROP INDEX vehicle_details_registration_index;
-- DROP INDEX vehicle_details_data_source_index;

-- Add contraints
ALTER TABLE vehicle_captures 
  ADD CONSTRAINT vehicle_captures_session_fk 
  FOREIGN KEY (session) 
  REFERENCES sessions(session);
  
ALTER TABLE vehicle_details 
  ADD CONSTRAINT vehicle_details_data_source_fk 
  FOREIGN KEY (data_source) 
  REFERENCES vehicle_details_data_sources(data_source);

--ALTER TABLE observations_meteorological 
--  ADD CONSTRAINT observations_meteorological_site_fk 
--  FOREIGN KEY (site) 
--  REFERENCES sites_meteorological(site);

-- ALTER TABLE sessions 
-- ADD CONSTRAINT site_met_fk 
-- FOREIGN KEY (site_met) 
-- REFERENCES sites_meteorological(site)
