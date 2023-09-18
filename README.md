# Rollercoaster_Evolution
Exploring the evolution of roller coaster design and construction from the perspective of amusement park visitors, circa 1850 - Present.  

### **Workflow** 

#### 0. Desired Data
* Data Ouput

  | tbl_name      | data_item   | key_id | build_source       |
  |   :---:       |   :---:     | :---:  |:---:               |
  | park_inventory| park_name   | FALSE  |park_info(park_url) |
  | park_inventory| ride_name   | FALSE  |park_info(park_url) |
  | park_inventory| park_url    | TRUE   |park_info(park_url) |
  | park_inventory| ride_url    | TRUE   |park_info(park_url) |
  | ride_inventory| ride_name   | FALSE  |park_info(park_url) |
  | ride_inventory| park_name   | FALSE  |park_info(park_url) | 
  | ride_specs    | ride_name   | FALSE  |ride_info(park_url) |
  | ride_specs    | park_name   | FALSE  |ride_info(park_url) | 
  | ride_specs    | ride_url    | TRUE   |ride_info(park_url) |
  | ride_specs    | park_url    | TRUE   |ride_info(park_url) | 
  | ride_specs    | ride_height | FALSE  |tbd    |
  | ride_specs    | ride_length | FALSE  |tbd    | 
  | ride_specs    | ride_speed  | FALSE  |tbd    | 
  | ride_specs    | ride_design | FALSE  |tbd    |
  | ride_status   | yr_opened   | FALSE  |park_info(park_url) |
  | ride_status   | yr_closed   | FALSE  |park_info(park_url) |
  | ride_status   | ride_status | FALSE  |park_info(park_url) |
  | crosswalk_A   | park_name   | FALSE  | build manually     |
  | crosswalk_A   | park_url    | TRUE   | build manually     |
  | crosswalk_B   | park_url    | TRUE   | tbd | 
  | crosswalk_B   | ride_name   | FALSE  | tbd | 
  | crosswalk_B   | ride_url    | TRUE   | tbd |

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
