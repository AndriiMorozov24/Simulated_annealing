
# Simulated Annealing for Flow-Shop Scheduling  

This repository contains an implementation of the **Simulated Annealing (SA) algorithm** for solving the **Flow-Shop Scheduling Problem**. The algorithm optimizes the **makespan** (total completion time) by scheduling tasks across multiple machines while ensuring that each task follows a sequential order from **Machine 1 to Machine N**.  

## How to Use  

1. **Provide Input Data**  
   - When prompted, enter the name of your input file (**line 3**).  
   - The input must be in **CSV format**, where:  
     - **Rows** represent tasks, with values indicating the processing time required on each machine.  
     - **Columns** represent machines, indexed sequentially from 1 to N.  
   - Tasks are processed in order, and a task on a given machine can only begin once the machine is available.  

2. **Configuring the Algorithm**  
   - The **number of probes per epoch** is controlled by the variable **k** (**line 7**). Adjusting this value influences the exploration-exploitation balance.  
   - You can modify the **initial temperature** and **final temperature** on **lines 8-9**, which affect the convergence behavior.  
   - The **temperature update formula** is defined on **line 66**—you may customize it based on your optimization needs.  

3. **Execution & Output**  
   - Once the program completes, the results will be saved in **"output.txt"**.  
   - The repository includes example **CSV datasets** and corresponding **sample outputs** for reference.  

## Contact  
For any questions or contributions, feel free to reach out at **andrii.morozov23@gmail.com**.  
