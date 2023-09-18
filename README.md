# Rollercoaster_Evolution
Exploring the evolution of roller coaster design and construction from the perspective of amusement park visitors, circa 1850 - Present.  

### **Workflow** 

#### 0. Described Data
All input and output data needed and generated, including a reference to the applicapble function that will generate or use the data in the code.  

  | tbl_name          | data_item   | key_id | function           |
  |   :---:           |   :---:     | :---:  |:---:               |
  | park_inventory.csv| park_name   | FALSE  |park_info(park_url) |
  | park_inventory.csv| ride_name   | FALSE  |park_info(park_url) |
  | park_inventory.csv| park_url    | TRUE   |park_info(park_url) |
  | park_inventory.csv| ride_url    | TRUE   |park_info(park_url) |
  | ride_inventory.csv| ride_name   | FALSE  |park_info(park_url) |
  | ride_inventory.csv| park_name   | FALSE  |park_info(park_url) | 
  | ride_specs.csv    | ride_name   | FALSE  |ride_info(ride_url) |
  | ride_specs.csv    | park_name   | FALSE  |ride_info(ride_url) | 
  | ride_specs.csv    | ride_url    | TRUE   |ride_info(ride_url) |
  | ride_specs.csv    | park_url    | TRUE   |ride_info(ride_url) | 
  | ride_specs.csv    | ride_height | FALSE  |ride_info(ride_url) |
  | ride_specs.csv    | ride_length | FALSE  |ride_info(ride_url) | 
  | ride_specs.csv    | ride_speed  | FALSE  |ride_info(ride_url) | 
  | ride_specs.csv    | ride_design | FALSE  |ride_info(ride_url) |
  | ride_status.csv   | yr_opened   | FALSE  |park_info(park_url) |
  | ride_status.csv   | yr_closed   | FALSE  |park_info(park_url) |
  | ride_status.csv   | ride_status | FALSE  |park_info(park_url) |
  | crosswalk_pr.csv  | park_url    | TRUE   |park_rides(park_url)| 
  | crosswalk_pr.csv  | ride_name   | FALSE  |park_rides(park_url)| 
  | crosswalk_pr.csv  | ride_url    | TRUE   |park_rides(park_url)|
  | selected_parks.csv| park_url    | TRUE   |built manually     |

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
