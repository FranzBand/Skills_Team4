# Settings
PRINT_SIM_PROGRESS = True


# Time values
DURATION_DAYS = 260
HOURS_IN_DAY = 9
DURATION_HOURS = HOURS_IN_DAY*DURATION_DAYS


# Data generation process
SEED = 37
PATIENT_DISTRIBUTIONS = [(('exponential', (HOURS_IN_DAY/17,)),  # Arrival time patient type I
                          ('normal', (25.71/60, 5.84/60))),  # Scan duration type I
                         (('exponential', (HOURS_IN_DAY/10,)),  # Arrival time patient type II
                          ('lognormal', (40.71/60, 12.07/60)))]  # Scan duration type II
NUM_MACHINES = 2


# REPRESENTATIONS
SCHEDULE_PATIENTS = 10
MAKE_SCAN = 20
