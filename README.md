# Rollercoaster_Evolution
Exploring the evolution of roller coaster design and construction from the perspective of amusement park visitors, circa 1850 - Present.  

### **Workflow** 

#### 0. Described Data
All input and output data needed and generated, including a reference to the applicapble function that will generate or use the data in the code.  

  | tbl_name      | data_item   | key_id | build_source       |
  |   :---:       |   :---:     | :---:  |:---:               |
  | park_inventory| park_name   | FALSE  |park_info(park_url) |
  | park_inventory| ride_name   | FALSE  |park_info(park_url) |
  | park_inventory| park_url    | TRUE   |park_info(park_url) |
  | park_inventory| ride_url    | TRUE   |park_info(park_url) |
  | ride_inventory| ride_name   | FALSE  |park_info(park_url) |
  | ride_inventory| park_name   | FALSE  |park_info(park_url) | 
  | ride_specs    | ride_name   | FALSE  |ride_info(ride_url) |
  | ride_specs    | park_name   | FALSE  |ride_info(ride_url) | 
  | ride_specs    | ride_url    | TRUE   |ride_info(ride_url) |
  | ride_specs    | park_url    | TRUE   |ride_info(ride_url) | 
  | ride_specs    | ride_height | FALSE  |ride_info(ride_url) |
  | ride_specs    | ride_length | FALSE  |ride_info(ride_url) | 
  | ride_specs    | ride_speed  | FALSE  |ride_info(ride_url) | 
  | ride_specs    | ride_design | FALSE  |ride_info(ride_url) |
  | ride_status   | yr_opened   | FALSE  |park_info(park_url) |
  | ride_status   | yr_closed   | FALSE  |park_info(park_url) |
  | ride_status   | ride_status | FALSE  |park_info(park_url) |
  | crosswalk_A   | park_name   | FALSE  |built manually      |
  | crosswalk_A   | park_url    | TRUE   |built manually      |
  | crosswalk_B   | park_url    | TRUE   |park_rides(park_url)| 
  | crosswalk_B   | ride_name   | FALSE  |park_rides(park_url)| 
  | crosswalk_B   | ride_url    | TRUE   |park_rides(park_url)|

#### 1. Build Data
  - [...]
  - [...]
  
#### 2. Tidy 
  - [...]
  - [...]
  
#### 3.  Explore
  - [...]
  - [...]
  
#### 4. Design Dashboard Layout
  - [...]
  - [...]
  
#### 5. Deploy Dashboard

### **Goals / Purpose**
