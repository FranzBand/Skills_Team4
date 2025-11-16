from simulation_part.constants import DURATION_DAYS
from simulation_part.sim.sim_objects.simulation_time import Time
from simulation_part.sim.simulation import Simulation

TIME_SLOT_TYPEI = 1
TIME_SLOT_TYPEII = 1


def run_simulation():
    end_time = Time(DURATION_DAYS+1, 0)
    sim = Simulation(TIME_SLOT_TYPEI, TIME_SLOT_TYPEII)
    while sim.time < end_time:
        sim.next_event()
    sim.run_analysis()


if __name__ == "__main__":
    run_simulation()
