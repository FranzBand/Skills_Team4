import numpy as np
import pandas as pd
from matplotlib import pyplot as plt

from simulation_part.constants import HOURS_IN_DAY, DURATION_DAYS


class DataCollector:
    """
    - Wait time 1:  Number of days until appointment from call
    - Wait time 2:  Time from appointment to actual treatment
    - Over time:    Time spend after 17.00
    - Idle time:    Time the machines do nothing
    """

    def __init__(self):
        self._curr_time = None

        self._wait_times1 = [[], []]
        self._wait_times2 = [[], []]
        self._over_time = [[], []]
        self._idle_time = [[], []]

        self._summary_stats = pd.DataFrame(
            columns=["slot1", "slot2", "wait1_1", "wait1_2", "wait2_1", "wait2_2", "over_1", "over_2", "idle_1", "idle_2"])

    def set_time(self, time):
        self._curr_time = time

    def log_patient_scheduled(self, patient_data):
        patient_type = patient_data['patient_type']

        day_called = self._curr_time.day
        day_scheduled = patient_data['scheduled_at'].day
        self._wait_times1[patient_type].append(day_scheduled-day_called)

    def log_patient_start_treatment(self, patient_data):
        patient_type = patient_data['patient_type']

        time_scheduled = patient_data['scheduled_at'].hour
        time_treated = self._curr_time.hour
        self._wait_times2[patient_type].append(time_treated-time_scheduled)

    def log_patient_ends_treatment(self, curr_patient, next_patient_time, ended_at, machine_id):
        if curr_patient['scheduled_at'].day < next_patient_time.day:
            self._over_time[machine_id].append(max(0, ended_at.hour-HOURS_IN_DAY))
            self._idle_time[machine_id].append(max(0, HOURS_IN_DAY-ended_at.hour))
        else:
            self._idle_time[machine_id].append(max(0, next_patient_time.hour-ended_at.hour))

    def end_experiment(self, slot1, slot2):
        stats = [
            slot1, slot2,
            np.mean(self._wait_times1[0]),
            np.mean(self._wait_times1[1]),
            np.mean(self._wait_times2[0]),
            np.mean(self._wait_times2[1]),
            np.mean(self._over_time[0]),
            np.mean(self._over_time[1]),
            np.sum(self._idle_time[0]) / DURATION_DAYS,
            np.sum(self._idle_time[1]) / DURATION_DAYS
        ]
        self._summary_stats.loc[len(self._summary_stats)] = stats

        self._wait_times1 = [[], []]
        self._wait_times2 = [[], []]
        self._over_time = [[], []]
        self._idle_time = [[], []]

    def run_time_slot_analysis(self, font_size=14):
        features = self._summary_stats.columns[2:]  # all columns except x and y

        for slot, wait1, wait2, over, idle in [["slot1", "wait1_1", "wait2_1", "over_1", "idle_1"], ["slot2", "wait1_2", "wait2_2", "over_2", "idle_2"]]:
            df_sorted = self._summary_stats.sort_values(slot)
            with pd.option_context("display.max_rows", None, "display.max_columns", None, "display.width", None, "display.expand_frame_repr", False):
                print(df_sorted.groupby(slot).agg({
                    wait1: "mean",
                    wait2: "mean",
                    over: "mean",
                    # .sum() / DURATION_DAYS is already done in end_experiment
                    idle: "mean",
                }))
            wait1_data = df_sorted.groupby(slot)[wait1].mean()
            wait2_data = df_sorted.groupby(slot)[wait2].mean()
            over_data = df_sorted.groupby(slot)[over].mean()
            # .sum() / DURATION_DAYS is already done in end_experiment
            idle_data = df_sorted.groupby(slot)[idle].mean()

            fig, ax1 = plt.subplots(figsize=(8, 6))

            ax1.plot(wait1_data.index, wait1_data.values, marker="o", color="tab:blue", label="Treatment time (days)")
            ax1.set_xlabel(f"Time slot Type {"I"*int(slot[-1])}", fontsize=font_size)
            ax1.set_ylabel("Time (days)", fontsize=font_size)
            ax1.tick_params(axis='both', labelsize=font_size)
            # ax1.tick_params(axis="y", labelcolor="tab:blue")

            ax2 = ax1.twinx()
            ax2.plot(wait2_data.index, wait2_data.values, marker="s", color="tab:red", label="Wait time (hours)")
            ax2.plot(over_data.index, over_data.values, marker="^", color="tab:green", label="Over time (hours)")
            ax2.plot(idle_data.index, idle_data.values, marker="^", color="tab:orange", label="Idle time (hours)")
            ax2.set_ylabel("Time (hours)", fontsize=font_size)
            ax2.tick_params(axis='both', labelsize=font_size)
            # ax2.tick_params(axis="y", labelcolor="tab:red")
            ax2.grid(True, linestyle="--", alpha=0.5)

            lines, labels = ax1.get_legend_handles_labels()
            lines2, labels2 = ax2.get_legend_handles_labels()
            ax1.legend(lines + lines2, labels + labels2, loc='upper center')

            ax1.set_ylim(bottom=0)
            ax2.set_ylim(bottom=0)

            # plt.title("Line plot of two features averaged over y (sorted by y)", fontsize=font_size)
            plt.show()

        # Plot each feature as a heatmap
        for feature in features:
            # Pivot to 2D array
            pivot = self._summary_stats.pivot(index="slot1", columns="slot2", values=feature)

            plt.figure(figsize=(8, 6))

            # Pass actual x and y coordinates to extent
            # extent = [xmin, xmax, ymin, ymax]
            x_unique = pivot.columns.values
            y_unique = pivot.index.values

            plt.imshow(
                pivot.values,
                origin="lower",
                cmap="viridis",
                aspect="auto",
                extent=[x_unique.min(), x_unique.max(), y_unique.min(), y_unique.max()]
            )

            plt.colorbar(label=feature)
            plt.title(f"{feature}")
            plt.xlabel("Time slot Type II (minutes)")
            plt.ylabel("Time slot Type I (minutes)")

            # Optional: set ticks explicitly to match x and y values
            plt.xticks(x_unique)
            plt.yticks(y_unique)

            plt.show()

    def run_analysis(self):
        treatments_machines1, treatments_machines2 = len(self._idle_time[0]), len(self._idle_time[1])
        calls_type1, calls_type2 = len(self._wait_times1[0]), len(self._wait_times1[1])
        idle_time1, idle_time2 = np.sum(self._idle_time[0])/DURATION_DAYS, np.sum(self._idle_time[1])/DURATION_DAYS

        print(3*"\n")
        print(f"The simulation lasted for {DURATION_DAYS} (working)days\n")
        print(f"         Type 1  Type 2")
        print(f"Patients {calls_type1:>6} {calls_type2:>6}")
        print(f"Machines {treatments_machines1:>6} {treatments_machines2:>6}")
        print(f"Idle     {idle_time1:6.2f} {idle_time2:6.2f}")

        _plot_hist(self._wait_times1[0], title="Days until treatment (Type I)", days=True, is_integer=True)
        _plot_hist(self._wait_times1[0], title="Days until treatment (Type II)", days=True, is_integer=True)
        _plot_hist(self._wait_times2[0], title="Treatment delay (TYPE I)", )
        _plot_hist(self._wait_times2[0], title="Treatment delay (TYPE II)", )
        _plot_hist(self._over_time[0], title="Over time (Machine I)", )
        _plot_hist(self._over_time[0], title="Over time (Machine II)", )
        _plot_hist(self._idle_time[0], title="Idle time (Machine I)", )
        _plot_hist(self._idle_time[0], title="Idle time (Machine II)", )
        plt.close('all')


def _plot_hist(data, title="", days=False, is_integer=False, font_size=14):
    fig, ax = plt.subplots(figsize=(8, 5))

    bins = np.arange(np.min(data) - 0.5, np.max(data) + 1.5, 1) if is_integer else None
    # align = "mid" if is_integer else None
    plt.hist(
        data,
        bins=bins,
        density=True,
        rwidth=1.0,
        linewidth=0,
        # align="mid",
        alpha=0.85
    )

    plt.title(title, fontsize=font_size, pad=10)
    if days:
        plt.xlabel("Number of days", fontsize=font_size)
    else:
        plt.xlabel("Time in hours", fontsize=font_size)
    plt.ylabel("Density", fontsize=font_size)

    if is_integer:
        ax.set_xticks(np.arange(np.min(data), np.max(data) + 1))

    ax.tick_params(axis="both", labelsize=font_size)

    plt.grid(True, linestyle="--", linewidth=0.6, alpha=0.7)

    plt.tight_layout()
    plt.savefig(title)
    # plt.show()
