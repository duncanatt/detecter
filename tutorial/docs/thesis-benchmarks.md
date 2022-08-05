# THESIS Experiments

---

## Monitors for Master and Slave processes ({++n = 2k++})

The following code modifications are required to run the experiments correctly;
these should be applied for each experiment run:
1. Set `evm_tracer:trace/2` to use the `set_on_spawn` flag for each process.

### Testing after refactoring that the experiment stops. Test are done on the MAC and are for testing purposes.

### Steady Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
     monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 2000 random outline -r 20 -ps 0.9 -pr 0.9 -d /Users/duncan/Dropbox/PhD/Development/detecter-private/runs/thesis/2_k/outline/decentralized/separate/all -wc 100 -b 200 -sr 1 -pa /Users/duncan/Dropbox/PhD/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 2000 random outline -r 20 -ps 0.9 -pr 0.9 -d /Users/duncan/Dropbox/PhD/Development/detecter-private/runs/thesis/2_k/outline/decentralized/merged/all -wc 100 -b 200 -sr 1 -pa /Users/duncan/Dropbox/PhD/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 2000 random inline -r 20 -ps 0.9 -pr 0.9 -d /Users/duncan/Dropbox/PhD/Development/detecter-private/runs/thesis/2_k/inline/all -wc 100 -b 200 -sr 1 -pa /Users/duncan/Dropbox/PhD/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 2000 random outline -r 20 -ps 0.9 -pr 0.9 -d /Users/duncan/Dropbox/PhD/Development/detecter-private/runs/thesis/2_k/outline/centralized/merged/all -wc 100 -b 200 -sr 1 -pa /Users/duncan/Dropbox/PhD/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 2000 random baseline -r 20 -ps 0.9 -pr 0.9 -d /Users/duncan/Dropbox/PhD/Development/detecter-private/runs/thesis/2_k/baseline -wc 100 -b 200 -sr 1 -pa /Users/duncan/Dropbox/PhD/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

---



















## Monitors for Master and Slave processes ({++n = 10k++})

The following code modifications are required to run the experiments correctly;
these should be applied for each experiment run:
1. Set `evm_tracer:trace/2` to use the `set_on_spawn` flag for each process.

### Steady Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 random outline -r 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/outline/decentralized/separate/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 random outline -r 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/outline/decentralized/merged/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 10000 random inline -r 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/inline/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 random outline -r 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/outline/centralized/merged/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 random baseline -r 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/baseline -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


### Pulse Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/outline/decentralized/separate/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/outline/decentralized/merged/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 10000 pulse inline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/inline/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/outline/centralized/merged/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 pulse baseline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/baseline -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```


### Burst Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/outline/decentralized/separate/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/outline/decentralized/merged/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 10000 burst inline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/inline/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/outline/centralized/merged/all -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 10000 burst baseline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/10_k/baseline -wc 100 -b 1000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

---



















## Monitors for Master and Slave processes ({++n = 20k++})

The following code modifications are required to run the experiments correctly;
these should be applied for each experiment run:
1. Set `evm_tracer:trace/2` to use the `set_on_spawn` flag for each process.

### Steady Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 random outline -r 200 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/outline/decentralized/separate/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 random outline -r 200 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/outline/decentralized/merged/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 20000 random inline -r 200 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/inline/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 random outline -r 200 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/outline/centralized/merged/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 random baseline -r 200 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/baseline -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


### Pulse Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/outline/decentralized/separate/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/outline/decentralized/merged/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 20000 pulse inline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/inline/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/outline/centralized/merged/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 pulse baseline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/baseline -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```


### Burst Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/outline/decentralized/separate/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/outline/decentralized/merged/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 20000 burst inline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/inline/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/outline/centralized/merged/all -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 20000 burst baseline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/20_k/baseline -wc 100 -b 2000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

---



















## Monitors for Master and Slave processes ({++n = 100k++})

The following code modifications are required to run the experiments correctly;
these should be applied for each experiment run:
1. Set `evm_tracer:trace/2` to use the `set_on_spawn` flag for each process.


### Steady Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 random outline -r 1000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/separate/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 random outline -r 1000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/merged/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 100000 random inline -r 1000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/inline/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 random outline -r 1000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/centralized/merged/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 random baseline -r 1000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/baseline -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


### Pulse Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/separate/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/merged/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 100000 pulse inline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/inline/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/centralized/merged/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] Baseline

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 pulse baseline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/baseline -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```


### Burst Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/separate/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
   ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/merged/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 100000 burst inline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/inline/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/centralized/merged/all -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 burst baseline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/baseline -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```


## Monitor for Master Process Only ({++n = 100k++})

The following code modifications are required to run the experiments correctly;
these should be applied for each experiment run:
1. Set `evm_tracer:trace/2` **not** to use the `set_on_spawn` flag for each
   process.

These modifications are done so that **only** the master process is traced and
monitored, and there are **no** tracers nor monitors on the slave processes.
We use this to compute the estimated cost of monitoring overhead on the slaves
only by subtracting from the total cost of monitoring the master and slave
processes the cost of monitoring just the master. By dividing the result by the
number of slaves, we obtain the amortized cost of monitor overhead per slave.


### Steady Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 random outline -r 1000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/separate/master_only -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 random outline -r 1000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/merged/master_only -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


### Pulse Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/separate/master_only -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/merged/master_only -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```


#### Burst Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/separate/master_only -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 100000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/100_k/outline/decentralized/merged/master_only -wc 100 -b 10000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

---




















## Monitors for Master and Slave processes ({++n = 200k++})

The following code modifications are required to run the experiments correctly;
these should be applied for each experiment run:
1. Set `evm_tracer:trace/2` to use the `set_on_spawn` flag for each process.


### Steady Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 random outline -r 2000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/separate/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 random outline -r 2000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/merged/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 200000 random inline -r 2000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/inline/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 random outline -r 2000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/centralized/merged/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 random baseline -r 2000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/baseline -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


### Pulse Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/separate/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/merged/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 200000 pulse inline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/inline/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/centralized/merged/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 pulse baseline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/baseline -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```


### Burst Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/separate/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/merged/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 200000 burst inline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/inline/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/centralized/merged/all -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 burst baseline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/baseline -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```


## Monitor for Master Process Only ({++n = 200k++})

The following code modifications are required to run the experiments correctly;
these should be applied for each experiment run:
1. Set `evm_tracer:trace/2` **not** to use the `set_on_spawn` flag for each
   process.

These modifications are done so that **only** the master process is traced and
monitored, and there are **no** tracers nor monitors on the slave processes.
We use this to compute the estimated cost of monitoring overhead on the slaves
only by subtracting from the total cost of monitoring the master and slave
processes the cost of monitoring just the master. By dividing the result by the
number of slaves, we obtain the amortized cost of monitor overhead per slave.


### Steady Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 random outline -r 2000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/separate/master_only -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 random outline -r 2000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/merged/master_only -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


### Pulse Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/separate/master_only -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/merged/master_only -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```


### Burst Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/separate/master_only -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 200000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/200_k/outline/decentralized/merged/master_only -wc 100 -b 20000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

---



















## Monitors for Master and Slave processes ({++n = 500k++})

The following code modifications are required to run the experiments correctly;
these should be applied for each experiment run:
1. Set `evm_tracer:trace/2` to use the `set_on_spawn` flag for each process.


### Steady Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 random outline -r 5000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/separate/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 random outline -r 5000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/merged/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 500000 random inline -r 5000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/inline/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 random outline -r 5000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/centralized/merged/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 random baseline -r 5000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/baseline -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


### Pulse Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/separate/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/merged/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 500000 pulse inline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/inline/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/centralized/merged/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 pulse baseline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/baseline -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```


### Burst Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/separate/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/merged/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 500000 burst inline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/inline/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/centralized/merged/all -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 burst baseline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/baseline -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```


## Monitor for Master Process Only ({++n = 500k++})

The following code modifications are required to run the experiments correctly;
these should be applied for each experiment run:
1. Set `evm_tracer:trace/2` **not** to use the `set_on_spawn` flag for each
   process.

These modifications are done so that **only** the master process is traced and
monitored, and there are **no** tracers nor monitors on the slave processes.
We use this to compute the estimated cost of monitoring overhead on the slaves
only by subtracting from the total cost of monitoring the master and slave
processes the cost of monitoring just the master. By dividing the result by the
number of slaves, we obtain the amortized cost of monitor overhead per slave.


### Steady Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 random outline -r 5000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/separate/master_only -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 random outline -r 5000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/merged/master_only -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


### Pulse Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/separate/master_only -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/merged/master_only -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```


### Burst Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/separate/master_only -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 500000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/500_k/outline/decentralized/merged/master_only -wc 100 -b 50000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

---




















## Monitors for Master and Slave processes ({++n = 1M++})

The following code modifications are required to run the experiments correctly;
these should be applied for each experiment run:
1. Set `evm_tracer:trace/2` to use the `set_on_spawn` flag for each process.

### Steady Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 random outline -r 10000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/separate/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 random outline -r 10000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/merged/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 1000000 random inline -r 10000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/inline/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 random outline -r 10000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/centralized/merged/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 random baseline -r 10000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/baseline -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


### Pulse Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/separate/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/merged/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 1000000 pulse inline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/inline/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/centralized/merged/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 pulse baseline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/baseline -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```


### Burst Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/separate/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/merged/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Inline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Check that `Makefile` weaved in the correct monitor specifications using the
       function `fun experiment_thesis:driver_monitors/1`.
    5. Execute `make weave`.

    ```bash
    (nohup python -u -m cli run 1000000 burst inline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/inline/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Centralized Outline (Merged Tracer and Monitor)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/centralized/merged/all -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Baseline*

    1. Update `runner.py` and set the `num_mons` variable to `coverage + 1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable both master and slave
       monitors.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 burst baseline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/baseline -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```


## Monitor for Master Process Only ({++n = 1M++})

The following code modifications are required to run the experiments correctly;
these should be applied for each experiment run:
1. Set `evm_tracer:trace/2` **not** to use the `set_on_spawn` flag for each
   process.

These modifications are done so that **only** the master process is traced and
monitored, and there are **no** tracers nor monitors on the slave processes.
We use this to compute the estimated cost of monitoring overhead on the slaves
only by subtracting from the total cost of monitoring the master and slave
processes the cost of monitoring just the master. By dividing the result by the
number of slaves, we obtain the amortized cost of monitor overhead per slave.


### Steady Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 random outline -r 10000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/separate/master_only -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 random outline -r 10000 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/merged/master_only -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i random_0.9_0.9 &) && tail -f nohup.out
    ```


### Pulse Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/separate/master_only -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 pulse outline -tu 100 -s 25 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/merged/master_only -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i pulse_0.9_0.9 &) && tail -f nohup.out
    ```


### Burst Load

- [ ] *Decentralized Outline (Separate Tracers and Monitors)*

    1. Update `runner.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==SEPARATE==} part and comment {==MERGED==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `async_mon`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/separate/master_only -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

- [ ] *Decentralized Outline (Merged Tracers and Monitors)*

    1. Update `runner_thesis.py` and set the `num_mons` variable to `1`.
    2. Update `cli.py`, uncomment {==MERGED==} part and comment {==SEPARATE==} part.
    3. Update `experiment_thesis:driver_monitors/1` and enable **only** master
       monitor version that communicates the `EXIT` message to the owner process. The
       owner process must be set to `experiment_thesis`.
    4. Execute `make`.

    ```bash
    (nohup python -u -m cli run 1000000 burst outline -tu 100 -p 100 -ps 0.9 -pr 0.9 -d /home/duncan/Development/detecter-private/runs/thesis/1_M/outline/decentralized/merged/master_only -wc 100 -b 100000 -sr 3 -pa /home/duncan/Development/detecter-private/ebin -m experiment_thesis -i burst_0.9_0.9 &) && tail -f nohup.out
    ```

---
