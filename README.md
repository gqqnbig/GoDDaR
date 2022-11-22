### GoDDaR
A tool for Static **D**eadlock **D**etection **a**nd **R**esolution in **Go** Programs.

### Requirements:

* ocaml
* dune
* [migoinfer](https://github.com/JujuYuki/gospal) (optional)

### Example usage
```
Usage: ./GoDDar [-v | -ds] [-p <process> | -m <MiGo file>]
-v Output debug information
-ds Select deadlock resolution algorithm (1 or 2)
-p Process this process
-m Convert and process MiGo file
-help  Display this list of options
--help  Display this list of options
```

Analyse CCS process:
```
$ dune exec GoDDaR -- -p 'a!.b?.0 || b!.a?.0'
---- 1 ----
    (a!.b?.0 || b!.a?.0)

Deadlocks:
---- 1 ----
    (a!.b?.0 || b!.a?.0)
Resolved:
    ((a!.0 || b?.0) || (b!.0 || a?.0))
```

Analyse MiGo type:
```
$ dune exec GoDDaR -- -m test/data/benchmark/bad-order-circular/main.migo
---- 1 ----
(t0!.t1?.0 || t1!.t0?.0)

Deadlocks:
---- 1 ----
(t0!.t1?.0 || t1!.t0?.0)
Resolved:
((t0!.0 || t1?.0) || (t1!.0 || t0?.0))
```
