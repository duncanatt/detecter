################################################################################
## Emulation configurations                                                   ##
################################################################################


#LOG_DIR=runs/dais/resp_prob_dist_20k
LOG_DIR=runs/fase/logs

## Gmiel ta' graphs. ##

# Random (Poisson process).
# APP_CMD=driver:start(master_slave, 10, {random, 10}, {sim, 10, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_1000__10__1000_0.5_0.5.csv"}])
# APP_CMD=async_mon:start_online({driver, start, [master_slave, 10, {random, 10}, {sim, 10, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_1000__10__1000_0.5_0.5.csv"}, {owner,async_mon}]]}, fun launcher:driver_monitors/1, [{num_monitors,11}])
#APP_CMD=async_mon:start_online({driver, start, [master_slave, 10, {random, 10}, {sim, 10, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_1000__10__1000_0.5_0.5.csv"}, {owner,async_mon}]]}, fun example:mfa_spec/1, [{num_monitors,11}])

# With monitors.
# APP_CMD=driver:start(master_slave, 1000, {random, 10}, {sim, 50, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_1000__50__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_random_1000__50__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_random_1000__50__1000_0.5_0.5.csv -l $(LOG_DIR)/gm_random_1000__50__1000_0.5_0.5_load.csv
# APP_CMD=async_mon:start_online({driver, start, [master_slave, 1000, {random, 10}, {sim, 50, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_1000__50__1000_0.5_0.5_monitors.csv"}, {load_log, "$(LOG_DIR)/gm_random_1000__50__1000_0.5_0.5_load_monitors.csv"}, {owner,async_mon}]]}, fun launcher:driver_monitors/1, [{num_monitors, 1001}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_random_1000__50__1000_0.5_0.5_monitors.csv -l $(LOG_DIR)/gm_random_1000__50__1000_0.5_0.5_load_monitors.csv

# APP_CMD=driver:start(master_slave, 100000, {random, 100}, {sim, 50, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5.csv -l $(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5_load.csv
# APP_CMD=async_mon:start_online({driver, start, [master_slave, 100000, {random, 100}, {sim, 50, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5_monitors.csv"}, {load_log, "$(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5_load_monitors.csv"}, {owner,async_mon}]]}, fun launcher:driver_monitors/1, [{num_monitors, 100001}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5_monitors.csv -l $(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5_load_monitors.csv


# To test monitors that have been synthesized.
#APP_CMD=async_mon:start_online({system, syn_test_p1, []}, fun example:mfa_spec/1, [{num_monitors, 1}]).
# APP_CMD=async_mon:start_online({system, syn_test_p1, []}, fun system:simple_mfa_spec/1, [{num_monitors, 1}]).

# Even though monitor dies, tracer cannot (unless its own process dies or its routing
# table is empty). This is because even though the analysis is complete, it may be the
# case that the trace needs to spawn another monitor for that same process on another
# fork event.


#APP_CMD=driver:start(master_slave, 1000, {random, 10}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_1000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 2000, {random, 20}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_2000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 3000, {random, 30}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_3000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 4000, {random, 40}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_4000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 5000, {random, 50}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_5000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 6000, {random, 60}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_6000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 7000, {random, 70}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_7000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 8000, {random, 80}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_8000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 9000, {random, 80}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_9000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 10000, {random, 100}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_10000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 15000, {random, 150}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_15000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 20000, {random, 200}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_20000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 25000, {random, 250}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_25000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 30000, {random, 300}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_30000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 35000, {random, 350}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_35000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 40000, {random, 400}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_40000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 50000, {random, 500}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_45000__10__1000_0.5_0.5.csv"}])
#APP_CMD=driver:start(master_slave, 60000, {random, 600}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_50000__10__1000_0.5_0.5.csv"}])


### Random (Poisson process). ###
# APP_CMD=driver:start(master_slave, 1000, {random, 10}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_1000__10__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_random_1000__10__1000_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_1000__10__1000_0.5_0.5_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_random_1000__10__1000_0.5_0.5.csv -l $(LOG_DIR)/gm_random_1000__10__1000_0.5_0.5_load.csv -r $(LOG_DIR)/gm_random_1000__10__1000_0.5_0.5_recv.csv
# APP_CMD=driver:start(master_slave, 10000, {random, 100}, {sim, 50, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_10000__100__50_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_random_10000__100__50_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_10000__100__50_0.5_0.5_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_random_10000__100__50_0.5_0.5.csv -l $(LOG_DIR)/gm_random_10000__100__50_0.5_0.5_load.csv -r $(LOG_DIR)/gm_random_10000__100__50_0.5_0.5_recv.csv
# APP_CMD=driver:start(master_slave, 10000, {random, 100}, {sim, 100, 0.9, 0.9}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_10000__100__100_0.9_0.9.csv"}, {load_log, "$(LOG_DIR)/gm_random_10000__100__100_0.9_0.9_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_10000__100__100_0.9_0.9_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_random_10000__100__100_0.5_0.5.csv -l $(LOG_DIR)/gm_random_10000__100__100_0.5_0.5_load.csv -r $(LOG_DIR)/gm_random_10000__100__100_0.5_0.5_recv.csv

### Random (Poisson process). ###
# APP_CMD=driver:start(master_slave, 1000, {random, 10}, {sim, 1000, 1.0, 1.0}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_1000__10__1000_1.0_1.0.csv"}, {load_log, "$(LOG_DIR)/gm_random_1000__10__1000_1.0_1.0_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_1000__10__1000_1.0_1.0_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_random_1000__10__1000_1.0_1.0.csv -l $(LOG_DIR)/gm_random_1000__10__1000_1.0_1.0_load.csv -r $(LOG_DIR)/gm_random_1000__10__1000_1.0_1.0_recv.csv
# APP_CMD=driver:start(master_slave, 10000, {random, 100}, {sim, 50, 1.0, 1.0}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_10000__100__50_1.0_1.0.csv"}, {load_log, "$(LOG_DIR)/gm_random_10000__100__50_1.0_1.0_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_10000__100__50_1.0_1.0_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_random_10000__100__50_1.0_1.0.csv -l $(LOG_DIR)/gm_random_10000__100__50_1.0_1.0_load.csv -r $(LOG_DIR)/gm_random_10000__100__50_1.0_1.0_recv.csv
# APP_CMD=driver:start(master_slave, 10000, {random, 100}, {sim, 100, 1.0, 1.0}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_10000__100__100_1.0_1.0.csv"}, {load_log, "$(LOG_DIR)/gm_random_10000__100__100_1.0_1.0_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_10000__100__100_1.0_1.0_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_random_10000__100__100_1.0_1.0.csv -l $(LOG_DIR)/gm_random_10000__100__100_1.0_1.0_load.csv -r $(LOG_DIR)/gm_random_10000__100__100_1.0_1.0_recv.csv


### Steady (Uniform distribution). ###
# APP_CMD=driver:start(master_slave, 1000, {steady, 100}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_steady_1000__100__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_steady_1000__100__1000_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_steady_1000__100__1000_0.5_0.5_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_steady_1000__100__1000_0.5_0.5.csv -l $(LOG_DIR)/gm_steady_1000__100__1000_0.5_0.5_load.csv -r $(LOG_DIR)/gm_steady_1000__100__1000_0.5_0.5_recv.csv
# APP_CMD=driver:start(master_slave, 1000, {steady, 100}, {sim, 50, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_steady_10000__100__50_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_steady_10000__100__50_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_steady_10000__100__50_0.5_0.5_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_steady_10000__100__50_0.5_0.5.csv -l $(LOG_DIR)/gm_steady_10000__100__50_0.5_0.5_load.csv -r $(LOG_DIR)/gm_steady_10000__100__50_0.5_0.5_recv.csv
# APP_CMD=driver:start(master_slave, 1000, {steady, 100}, {sim, 100, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_steady_10000__100__100_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_steady_10000__100__100_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_steady_10000__100__100_0.5_0.5_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_steady_10000__100__100_0.5_0.5.csv -l $(LOG_DIR)/gm_steady_10000__100__100_0.5_0.5_load.csv -r $(LOG_DIR)/gm_steady_10000__100__100_0.5_0.5_recv.csv

### Pulse (Normal distribution). ###
# APP_CMD=driver:start(master_slave, 1000, {pulse, 100, 20}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_pulse_1000__100_20__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_pulse_1000__100_20__1000_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_pulse_1000__100_20__1000_0.5_0.5_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_pulse_1000__100_20__1000_0.5_0.5.csv -l $(LOG_DIR)/gm_pulse_1000__100_20__1000_0.5_0.5_load.csv -r $(LOG_DIR)/gm_pulse_1000__100_20__1000_0.5_0.5_recv.csv
# APP_CMD=driver:start(master_slave, 10000, {pulse, 100, 20}, {sim, 50, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_pulse_10000__100_20__50_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_pulse_10000__100_20__50_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_pulse_10000__100_20__50_0.5_0.5_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_pulse_10000__100_20__50_0.5_0.5.csv -l $(LOG_DIR)/gm_pulse_10000__100_20__50_0.5_0.5_load.csv -r $(LOG_DIR)/gm_pulse_10000__100_20__50_0.5_0.5_recv.csv
# APP_CMD=driver:start(master_slave, 10000, {pulse, 100, 25}, {sim, 100, 0.9, 0.9}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_pulse_10000__100_25__100_0.9_0.9.csv"}, {load_log, "$(LOG_DIR)/gm_pulse_10000__100_25__100_0.9_0.9_load.csv"}, {recv_log, "$(LOG_DIR)/gm_pulse_10000__100_25__100_0.9_0.9_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_pulse_10000__100_20__100_0.5_0.5.csv -l $(LOG_DIR)/gm_pulse_10000__100_20__100_0.5_0.5_load.csv -r $(LOG_DIR)/gm_pulse_10000__100_20__100_0.5_0.5_recv.csv

### Burst (Log-normal distribution). ###
# APP_CMD=driver:start(master_slave, 1000, {burst, 100, 1000}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_burst_1000__100_1000__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_burst_1000__100_1000__1000_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_burst_1000__100_1000__1000_0.5_0.5_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_burst_1000__100_1000__1000_0.5_0.5.csv -l $(LOG_DIR)/gm_burst_1000__100_1000__1000_0.5_0.5_load.csv -r $(LOG_DIR)/gm_burst_1000__100_1000__1000_0.5_0.5_recv.csv
# APP_CMD=driver:start(master_slave, 10000, {burst, 100, 1000}, {sim, 50, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_burst_10000__100_1000__50_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_burst_10000__100_1000__50_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_burst_10000__100_1000__50_0.5_0.5_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_burst_10000__100_1000__50_0.5_0.5.csv -l $(LOG_DIR)/gm_burst_10000__100_1000__50_0.5_0.5_load.csv -r $(LOG_DIR)/gm_burst_10000__100_1000__50_0.5_0.5_recv.csv
# APP_CMD=driver:start(master_slave, 10000, {burst, 100, 100}, {sim, 100, 0.9, 0.9}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_burst_10000__100_100__100_0.9_0.9.csv"}, {load_log, "$(LOG_DIR)/gm_burst_10000__100_100__100_0.9_0.9_load.csv"}, {recv_log, "$(LOG_DIR)/gm_burst_10000__100_100__100_0.9_0.9_recv.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/gm_burst_10000__100_1000__100_0.5_0.5.csv -l $(LOG_DIR)/gm_burst_10000__100_1000__100_0.5_0.5_load.csv -r $(LOG_DIR)/gm_burst_10000__100_1000__100_0.5_0.5_recv.csv

### Poisson process for different probabilities 100000. ###
# APP_CMD=driver:start(master_slave, 100000, {random, 1000}, {sim, 100, 0.1, 0.1}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.1_0.1.csv"}, {load_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.1_0.1_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.1_0.1_recv.csv"}])
# APP_CMD=driver:start(master_slave, 100000, {random, 1000}, {sim, 100, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.5_0.5_recv.csv"}])
# APP_CMD=driver:start(master_slave, 100000, {random, 1000}, {sim, 100, 0.9, 0.9}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.9_0.9.csv"}, {load_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.9_0.9_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.9_0.9_recv.csv"}])

### Poisson process for different probabilities 10000. ###
# APP_CMD=driver:start(master_slave, 10000, {random, 100}, {sim, 100, 0.1, 0.1}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_10000__100__100_0.1_0.1.csv"}, {load_log, "$(LOG_DIR)/gm_random_10000__100__100_0.1_0.1_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_10000__100__100_0.1_0.1_recv.csv"}])
# APP_CMD=driver:start(master_slave, 10000, {random, 100}, {sim, 100, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_10000__100__100_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_random_10000__100__100_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_10000__100__100_0.5_0.5_recv.csv"}])
# APP_CMD=driver:start(master_slave, 10000, {random, 100}, {sim, 100, 0.9, 0.9}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_10000__100__100_0.9_0.9.csv"}, {load_log, "$(LOG_DIR)/gm_random_10000__100__100_0.9_0.9_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_10000__100__100_0.9_0.9_recv.csv"}])


### Poisson process for different probabilities 20000. To fit the distributions. ###
# APP_CMD=driver:start(master_slave, 20000, {random, 200}, {sim, 100, 0.1, 0.1}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_20000__200__100_0.1_0.1.csv"}, {load_log, "$(LOG_DIR)/gm_random_20000__200__100_0.1_0.1_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_20000__200__100_0.1_0.1_recv.csv"}])
# APP_CMD=driver:start(master_slave, 20000, {random, 200}, {sim, 100, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_20000__200__100_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/gm_random_20000__200__100_0.5_0.5_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_20000__200__100_0.5_0.5_recv.csv"}])
#APP_CMD=driver:start(master_slave, 20000, {random, 200}, {sim, 100, 0.9, 0.9}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_20000__200__100_0.9_0.9.csv"}, {load_log, "$(LOG_DIR)/gm_random_20000__200__100_0.9_0.9_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_20000__200__100_0.9_0.9_recv.csv"}])


### Poisson process idle time monitor. ####
# APP_CMD=driver:start(master_slave, 1, {random, 1}, {sim, 5, 0.9, 0.9}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_10000__100__100_0.9_0.9.csv"}, {load_log, "$(LOG_DIR)/gm_random_10000__100__100_0.9_0.9_load.csv"}, {recv_log, "$(LOG_DIR)/gm_random_10000__100__100_0.9_0.9_recv.csv"}])
# APP_CMD=experiment_dais:start_online({driver, start, [master_slave, 1, {random, 1}, {sim, 5, 0.9, 0.9}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5_monitors.csv"}, {load_log, "$(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5_load_monitors.csv"}, {owner,experiment_dais}]]}, fun experiment_dais:driver_monitors/1, [{num_monitors, 1}])
# APP_CMD=experiment_dais:start_online({driver, start, [master_slave, 100, {random, 10}, {sim, 100, 0.9, 0.9}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5_monitors.csv"}, {load_log, "$(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5_load_monitors.csv"}, {owner,experiment_dais}]]}, fun experiment_dais:driver_monitors/1, [{num_monitors, 100}, {idle_log, "$(LOG_DIR)/gm_random_100000__50__1000_0.5_0.5_idle.csv"}])

#         experiment_dais:start_online({driver, start, [master_slave, 100, {pulse,1,25}, {sim,100,0.9,0.9}, [{int_len_ms,1000},{seed,{25,9,83}},{load_log,"/Users/duncan/Dropbox/PhD/Development/detecter/runs/dais/exp_master_slaves/idle/pulse_0.9_0.9/sim__10__pulse_1_25__100_0.9_0.9__outline__load__1.csv"},{stat_log,"/Users/duncan/Dropbox/PhD/Development/detecter/runs/dais/exp_master_slaves/idle/pulse_0.9_0.9/sim__10__pulse_1_25__100_0.9_0.9__outline__load__1.csv"}, {owner,async_mon}]]}, fun launcher:driver_monitors/1, [{num_monitors, 11},{idle_log, "/Users/duncan/Dropbox/PhD/Development/detecter/runs/dais/exp_master_slaves/idle/pulse_0.9_0.9/sim__10__pulse_1_25__100_0.9_0.9__outline__idle__1.csv"}])

## ---- END GMIEL TA' GRAPHS --- ##

## Lightweight tests ##

# Random (Poisson process).
# APP_CMD=driver:start(master_slave, 1000, {random, 10}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/random_1000_10__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/random_1000_10__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/random_1000_10__1000_0.5_0.5.csv -l $(LOG_DIR)/random_1000_10__1000_0.5_0.5_load.csv
# APP_CMD=driver:start(master_slave, 10000, {random, 100}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/random_10000_100__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/random_10000_100__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/random_10000_100__1000_0.5_0.5.csv -l $(LOG_DIR)/random_10000_100__1000_0.5_0.5_load.csv

# Steady (Uniform distribution).
# APP_CMD=driver:start(master_slave, 1000, {steady, 100}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/steady_1000_100__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/steady_1000_100__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/steady_1000_100__1000_0.5_0.5.csv -l $(LOG_DIR)/steady_1000_100__1000_0.5_0.5_load.csv
# APP_CMD=driver:start(master_slave, 10000, {steady, 100}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/steady_10000_100__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/steady_10000_100__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/steady_1000_100__1000_0.5_0.5.csv -l $(LOG_DIR)/steady_1000_100__1000_0.5_0.5_load.csv

# Pulse (Normal distribution).
# APP_CMD=driver:start(master_slave, 1000, {pulse, 100, 20}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/pulse_1000_100_20__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/pulse_1000_100_20__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/pulse_1000_100_20__1000_0.5_0.5.csv -l $(LOG_DIR)/pulse_1000_100_20__1000_0.5_0.5_load.csv
# APP_CMD=driver:start(master_slave, 10000, {pulse, 100, 20}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/pulse_10000_100_20__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/pulse_10000_100_20__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/pulse_10000_100_20__1000_0.5_0.5.csv -l $(LOG_DIR)/pulse_10000_100_20__1000_0.5_0.5_load.csv

# Burst (Log-normal distribution).
# APP_CMD=driver:start(master_slave, 1000, {burst, 100, 2000}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/burst_1000_100_2000__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/burst_1000_100_2000__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/burst_1000_100_2000__1000_0.5_0.5.csv -l $(LOG_DIR)/burst_1000_100_2000__1000_0.5_0.5_load.csv
# APP_CMD=driver:start(master_slave, 10000, {burst, 100, 2000}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/burst_10000_100_2000__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/burst_10000_100_2000__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/burst_10000_100_2000__1000_0.5_0.5.csv -l $(LOG_DIR)/burst_10000_100_2000__1000_0.5_0.5_load.csv


## Heavyweight tests ##

# Random (Poisson process).
# APP_CMD=driver:start(master_slave, 100000, {random, 100}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/random_100000_100__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/random_100000_100__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/random_100000_100__1000_0.5_0.5.csv -l $(LOG_DIR)/random_100000_100__1000_0.5_0.5_load.csv
##APP_CMD=driver:start(master_slave, 1000000, {random, 1000}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/random_1000000_1000__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/random_1000000_1000__1000_0.5_0.5_load.csv"}])
##PLT_CMD=main.py -s $(LOG_DIR)/random_1000000_1000__1000_0.5_0.5.csv -l $(LOG_DIR)/random_1000000_1000__1000_0.5_0.5_load.csv

# Steady (Uniform distribution).
# APP_CMD=driver:start(master_slave, 100000, {steady, 1000}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/steady_100000_1000__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/steady_100000_1000__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/steady_100000_1000__1000_0.5_0.5.csv -l $(LOG_DIR)/steady_100000_1000__1000_0.5_0.5_load.csv
##APP_CMD=driver:start(master_slave, 1000000, {steady, 1000}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/steady_1000000_1000__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/steady_1000000_1000__1000_0.5_0.5_load.csv"}])
##PLT_CMD=main.py -s $(LOG_DIR)/steady_1000000_1000__1000_0.5_0.5.csv -l $(LOG_DIR)/steady_1000000_1000__1000_0.5_0.5_load.csv

# Pulse (Normal distribution).
#APP_CMD=driver:start(master_slave, 100000, {pulse, 1000, 200}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/pulse_100000_1000_200__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/pulse_100000_1000_200__1000_0.5_0.5_load.csv"}])
#PLT_CMD=main.py -s $(LOG_DIR)/pulse_100000_1000_200__1000_0.5_0.5.csv -l $(LOG_DIR)/pulse_100000_1000_200__1000_0.5_0.5_load.csv
#APP_CMD=driver:start(master_slave, 1000000, {pulse, 1000, 200}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/pulse_1000000_1000_200__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/pulse_1000000_1000_200__1000_0.5_0.5_load.csv"}])
#PLT_CMD=main.py -s $(LOG_DIR)/pulse_1000000_1000_200__1000_0.5_0.5.csv -l $(LOG_DIR)/pulse_1000000_1000_200__1000_0.5_0.5_load.csv

# Burst (Log-normal distribution).
#APP_CMD=driver:start(master_slave, 100000, {burst, 1000, 20000}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/burst_100000_1000_20000__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/burst_100000_1000_20000__1000_0.5_0.5_load.csv"}])
#PLT_CMD=main.py -s $(LOG_DIR)/burst_100000_1000_20000__1000_0.5_0.5.csv -l $(LOG_DIR)/burst_100000_1000_20000__1000_0.5_0.5_load.csv
#APP_CMD=driver:start(master_slave, 1000000, {burst, 1000, 20000}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/burst_1000000_1000_20000__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/burst_1000000_1000_20000__1000_0.5_0.5_load.csv"}])
#PLT_CMD=main.py -s $(LOG_DIR)/burst_1000000_1000_20000__1000_0.5_0.5.csv -l $(LOG_DIR)/burst_1000000_1000_20000__1000_0.5_0.5_load.csv


## Full probability tests ##

# Random (Poisson process).
# APP_CMD=driver:start(master_slave, 1000000, {random, 1000}, {sim, 1000, 1.0, 1.0}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/random_1000000_1000__1000_1.0_1.0.csv"}, {load_log, "$(LOG_DIR)/random_1000000_1000__1000_1.0_1.0_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/random_1000000_1000__1000_1.0_1.0.csv -l $(LOG_DIR)/random_1000000_1000__1000_1.0_1.0_load.csv

# Steady (Uniform distribution).
# APP_CMD=driver:start(master_slave, 1000000, {steady, 1000}, {sim, 1000, 1.0, 1.0}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/steady_1000000_1000__1000_1.0_1.0.csv"}, {load_log, "$(LOG_DIR)/steady_1000000_1000__1000_1.0_1.0_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/steady_1000000_1000__1000_1.0_1.0.csv -l $(LOG_DIR)/steady_1000000_1000__1000_1.0_1.0_load.csv

# Pulse (Normal distribution).
# APP_CMD=driver:start(master_slave, 1000000, {pulse, 1000, 200}, {sim, 1000, 1.0, 1.0}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/pulse_1000000_1000_200__1000_1.0_1.0.csv"}, {load_log, "$(LOG_DIR)/pulse_1000000_1000_200__1000_1.0_1.0_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/pulse_1000000_1000_200__1000_1.0_1.0.csv -l $(LOG_DIR)/pulse_1000000_1000_200__1000_1.0_1.0_load.csv

# Burst (Log-normal distribution).
# APP_CMD=driver:start(master_slave, 1000000, {burst, 1000, 20000}, {sim, 1000, 1.0, 1.0}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/burst_1000000_1000_20000__1000_1.0_1.0.csv"}, {load_log, "$(LOG_DIR)/burst_1000000_1000_20000__1000_1.0_1.0_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/burst_1000000_1000_20000__1000_1.0_1.0.csv -l $(LOG_DIR)/burst_1000000_1000_20000__1000_1.0_1.0_load.csv


## Tests oxxeni ##

# Random (Poisson process).
# APP_CMD=driver:start(master_slave, 2000000, {random, 2000}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/random_2000000_2000__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/random_2000000_2000__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/random_2000000_2000__1000_0.5_0.5.csv -l $(LOG_DIR)/random_2000000_2000__1000_0.5_0.5_load.csv
# APP_CMD=driver:start(master_slave, 2000000, {random, 2000}, {sim, 1000, 1.0, 1.0}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/random_2000000_2000__1000_1.0_1.0.csv"}, {load_log, "$(LOG_DIR)/random_2000000_2000__1000_1.0_1.0_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/random_2000000_2000__1000_1.0_1.0.csv -l $(LOG_DIR)/random_2000000_2000__1000_1.0_1.0_load.csv

# Steady (Uniform distribution).
#APP_CMD=driver:start(master_slave, 2000000, {steady, 1000}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/steady_2000000_1000__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/steady_2000000_1000__1000_0.5_0.5_load.csv"}])
#PLT_CMD=main.py -s $(LOG_DIR)/steady_2000000_1000__1000_0.5_0.5.csv -l $(LOG_DIR)/steady_2000000_1000__1000_0.5_0.5_load.csv
# APP_CMD=driver:start(master_slave, 2000000, {steady, 1000}, {sim, 1000, 1.0, 1.0}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/steady_2000000_1000__1000_1.0_1.0.csv"}, {load_log, "$(LOG_DIR)/steady_2000000_1000__1000_1.0_1.0_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/steady_2000000_1000__1000_1.0_1.0.csv -l $(LOG_DIR)/steady_2000000_1000__1000_1.0_1.0_load.csv

# Pulse (Normal distribution).
# APP_CMD=driver:start(master_slave, 2000000, {pulse, 1000, 200}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/pulse_2000000_1000_200__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/pulse_2000000_1000_200__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/pulse_2000000_1000_200__1000_0.5_0.5.csv -l $(LOG_DIR)/pulse_2000000_1000_200__1000_0.5_0.5_load.csv
# APP_CMD=driver:start(master_slave, 2000000, {pulse, 1000, 200}, {sim, 1000, 1.0, 1.0}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/pulse_2000000_1000_200__1000_1.0_1.0.csv"}, {load_log, "$(LOG_DIR)/pulse_2000000_1000_200__1000_1.0_1.0_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/pulse_2000000_1000_200__1000_1.0_1.0.csv -l $(LOG_DIR)/pulse_2000000_1000_200__1000_1.0_1.0_load.csv

# Burst (Log-normal distribution).
# APP_CMD=driver:start(master_slave, 2000000, {burst, 1000, 20000}, {sim, 1000, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/burst_2000000_1000_20000__1000_0.5_0.5.csv"}, {load_log, "$(LOG_DIR)/burst_2000000_1000_20000__1000_0.5_0.5_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/burst_2000000_1000_20000__1000_0.5_0.5.csv -l $(LOG_DIR)/burst_2000000_1000_20000__1000_0.5_0.5_load.csv
# APP_CMD=driver:start(master_slave, 2000000, {burst, 1000, 20000}, {sim, 1000, 1.0, 1.0}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/burst_2000000_1000_20000__1000_1.0_1.0.csv"}, {load_log, "$(LOG_DIR)/burst_2000000_1000_20000__1000_1.0_1.0_load.csv"}])
# PLT_CMD=main.py -s $(LOG_DIR)/burst_2000000_1000_20000__1000_1.0_1.0.csv -l $(LOG_DIR)/burst_2000000_1000_20000__1000_1.0_1.0_load.csv



# erl -pa ebin -i include -eval 'async_mon:start_online({driver, start, [master_slave, 10, {random, 10}, {sim, 10, 0.5, 0.5}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "runs/monitors_cmp/gm_random_1000__10__1000_0.5_0.5.csv"}, {owner,async_mon}]]}, fun launcher:driver_monitors/1, [{num_monitors,11}])'
# Result = weaver:weave("src/models", "include", "ebin2", fun({slave, init, [Id, Chunks]}) -> {ok, fun Mon(Event) -> Mon end}; (_) -> undefined end).

# weaver:weave_file("src/main.erl", "include", "ebin", fun({slave, init, [Id, Chunks]}) -> {ok, fun Mon(Event) -> Mon end}; (_) -> undefined end, fun(_) -> true end).

### OOPSLA merged monitor experiments ###

## Random ##
#APP_CMD=async_mon:start_online({driver, start, [master_slave, 100000, {random, 1000}, {sim, 100, 0.9, 0.9}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.9_0.9_stat.csv"}, {load_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.9_0.9_stat_load.csv"}, {owner,async_mon}]]}, fun experiment_oopsla:driver_monitors/1, [{num_monitors, 100001}])
#APP_CMD=experiment_oopsla:start_online({driver, start, [master_slave, 100000, {random, 1000}, {sim, 100, 0.9, 0.9}, [{seed, {25, 9, 83}}, {int_len_ms, 1000}, {stat_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.9_0.9_monitors.csv"}, {load_log, "$(LOG_DIR)/gm_random_100000__1000__100_0.9_0.9_load_monitors.csv"}, {owner,experiment_oopsla}]]}, fun experiment_oopsla:driver_monitors/1, [{num_monitors, 100001}])

### Cowboy experiments ###
APP_CMD=experiment_cowboy:start()

################################################################################
## Project configurations                                                     ##
################################################################################

BIN=detecter/ebin
# BIN=ebin2
INCLUDE=detecter/include
SRC=detecter/src
TEST=detecter/test
RES=detecter/resources

SCRIPTS=scripts
VENV=$(SCRIPTS)/venv

# Set shell to bash to use certain commands such as source.
SHELL=/bin/bash

define recursive
	$(shell find $(1) -name "*.$(2)")
endef

all: compile

compile: clean
	mkdir -p $(BIN)
	erlc -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)
	#cp $(RES)/* $(BIN)

analyze:
	 dialyzer -pa $(BIN) -I $(INCLUDE) $(call recursive,$(SRC),erl)

compile-test: clean
	mkdir -p $(BIN)
	erlc -DTEST -pa $(BIN) +debug_info -I $(INCLUDE) -o $(BIN) $(call recursive,$(SRC),erl)
	erlc -DTEST -pa $(BIN) -I $(INCLUDE) -o $(BIN) $(call recursive,$(TEST),erl)

test: compile-test
	#erl -noshell -pa ebin -eval 'eunit:test(log_tracer_test, [verbose])' -s init stop
	erl -noshell -pa ebin -eval 'eunit:test(tracer_test, [verbose])' -s init stop

weave: compile
# 	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", "$(INCLUDE)", "$(BIN)", fun launcher:driver_monitors/1, fun launcher:filter_spec/1).' -s init stop
# 	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", fun launcher:driver_monitors/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}, {filter, fun launcher:filter_spec/1}]).' -s init stop
# 	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", fun experiment_dais:mon_log/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}, {filter, fun launcher:filter_spec/1}]).' -s init stop
#	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", fun experiment_dais:driver_monitors/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}, {filter, fun launcher:filter_spec/1}]).' -s init stop
#	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", fun experiment_oopsla:driver_monitors/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}, {filter, fun launcher:filter_spec/1}]).' -s init stop
#	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/models", fun experiment_ifm:driver_monitors/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}, {filter, fun launcher:filter_spec/1}]).' -s init stop
	erl -noshell -pa ebin -eval 'weaver:weave("/Users/duncan/Dropbox/PhD/Development/detecter-inline/src/system", fun example_1:mfa_spec/1, [{outdir, "/Users/duncan/Dropbox/PhD/Development/detecter-inline/ebin"}, {i, "/Users/duncan/Dropbox/PhD/Development/detecter-inline/include"}, {filter, fun monitor:filter/1}, erl]).' -s init stop

weave-cowboy: compile
	erl -noshell -pa ebin -eval 'hml_eval:compile("examples/cowboy_mon.hml", [{outdir, "ebin"}]).' -s init stop
	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/ranch", fun cowboy_mon:mfa_spec/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}]).' -s init stop
	erl -noshell -pa ebin -eval 'weaver:weave("$(SRC)/cowboy", fun cowboy_mon:mfa_spec/1, [{outdir, "$(BIN)"}, {i, "$(INCLUDE)"}]).' -s init stop

run:
	mkdir -p $(LOG_DIR)
	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -noshell -eval '$(APP_CMD)' -s init stop
# 	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -noshell -eval '$(APP_CMD)'
# 	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -eval '$(APP_CMD)'

run-forever:
	mkdir -p $(LOG_DIR)
	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -noshell -eval '$(APP_CMD)'

plot: run
	source $(VENV)/bin/activate; python $(SCRIPTS)/$(PLT_CMD)

load:
	erl +S 4 +SDcpu 2 +P 134217727 -pa $(BIN)/ -eval 'code:ensure_modules_loaded([ascii_writer,async_mon,async_tracer_test,build,client,collector,common,csv_writer,distr,driver,echo_protocol,events,evm_tracer,gen_file_poller,gen_file_poller_impl,gen_looper,gen_looper_impl,hml_eval,hml_lexer,hml_lint,hml_parser,launcher,log,log_eval,log_lexer,log_parser,log_poller,log_tracer,main,master,monitor,opts,server,slave,stats,system,trace_lib,tracer,tracer_monitor,util,weaver,example_1,simple]).'

clean:
	rm -rf $(BIN)/*.beam $(BIN)/*.E $(BIN)/*.tmp erl_crash.dump $(BIN)/*.app


# Running the Erlang example.

## 1. Compile HML file.
# hml_eval:compile("props/server_props.hml", [{outdir,"ebin"}, v]).

## 2. Weave system (start dir: examples/erlang).
# weaver:weave("src/demo", fun server_props:mfa_spec/1, [{outdir,"ebin"}]).

## 3. Run system.
# calc_server:start(ok).
# calc_server:start(buggy).

# Running the Elixir example.

## 1. Compile HML file.
# :hml_eval.compile('props/server_props.hml', [{:outdir,'ebin'}, :v])

## 2. Run outline monitoring.
# :monitor.start_online({Elixir.Demo.CalcServer,:start,[:ok]}, &:server_props.mfa_spec/1, [])
# :monitor.start_online({Elixir.Demo.CalcServer,:start,[:buggy]}, &:server_props.mfa_spec/1, [{:analysis,:external}])


# Running the Outline example.

## 2. Run the outline monitor from the Erlang console (start dir: examples/erlang).
# It's ok for the log file not to exist. The file poller will pick it automatically once it does.
# monitor:start_offline("../../trace.log", pid(0,102,0), fun server_props:mfa_spec/1, []).

## 1. Run the Python script (start dir: examples/python/src).
# python -m demo.calc_server ok ../../../trace.log
# python -m demo.calc_server buggy ../../../trace.log

