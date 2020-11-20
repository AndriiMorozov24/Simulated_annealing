# Simulated_annealing
 simulated annealing algorithm for the flow-shop scheduling problem
andrii.morozov23@gmail.com 

Enter name of your input file. (3rd line)
To start calculations, provide data (4th line), it must be .csv format.
Rows must contain units of time for particular task on particular machine.
Columns contain machine number.
Each task should go from machine 1 to machine N (depends how many you set).
Task on particular machine can be done only if machine is free.
Fitness function (27th line) take care of it (makespan).

You can regulate speed of the program changing number of probes in the epoch by changing variable k. (7th line)
Also you can change start and end value of the temprature. (8-9th line)
66th line is responsible for formula that changes the tempreture, feel free to set it via your needs.

At the end will be created file called "output.txt" with results.
