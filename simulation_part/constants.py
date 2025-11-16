# Data generation process
from pyarrow.lib import Type_INT16

SEED = 37
PATIENT_DISTRIBUTIONS = [(('exponential', (1,)),  # Arrival time patient type I
                          ('normal', (1, 0.2))),  # Scan duration type I
                         (('exponential', (1,)),  # Arrival time patient type II
                          ('normal', (1, 0.2)))]  # Scan duration type II
NUM_MACHINES = 2


# Time values
DURATION_DAYS = 10
HOURS_IN_DAY = 9
DURATION_HOURS = HOURS_IN_DAY*DURATION_DAYS


# REPRESENTATIONS
SCHEDULE_PATIENTS = 10
MAKE_SCAN = 20
