import numpy as np

from simulation_part.constants import DURATION_DAYS
from simulation_part.sim.analysis.data_collector import DataCollector
from simulation_part.sim.sim_objects.simulation_time import Time
from simulation_part.sim.simulation import Simulation

TIME_SLOT_TYPEI = 28   # In minutes
TIME_SLOT_TYPEII = 45  # In minutes

TIME_SLOTS_TYPEI = np.arange(20, 31)      # In minutes
TIME_SLOTS_TYPEII = np.arange(30, 51, 2)  # In minutes

SHARED_MACHINES = True

def run_simulation():
    data_collector = DataCollector()
    end_time = Time(DURATION_DAYS+1, 0)
    sim = Simulation(TIME_SLOT_TYPEI/60, TIME_SLOT_TYPEII/60, data_collector, bool_shared_machines=SHARED_MACHINES)
    while sim.time < end_time:
        sim.next_event()
    data_collector.run_analysis()


def run_time_slot_simulation():
    """
    You probably want to set "PRINT_SIM_PROGRESS = False" when running this function
    """
    data_collector = DataCollector()

    for time_slot1 in TIME_SLOTS_TYPEI:
        for time_slot2 in TIME_SLOTS_TYPEII:
            end_time = Time(DURATION_DAYS+1, 0)
            sim = Simulation(time_slot1/60, time_slot2/60, data_collector, seed=17*time_slot1+3*time_slot2, bool_shared_machines=False)
            while sim.time < end_time:
                sim.next_event()
            data_collector.end_experiment(time_slot1, time_slot2)
    data_collector.run_time_slot_analysis()


if __name__ == "__main__":
    run_simulation()
    run_time_slot_simulation()
