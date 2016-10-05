% Number encoding Definitions

-define(NUMBER_EXTENSION_NONE, 1).
-define(NUMBER_NATURE_INTERNATIONAL, 1).
-define(NUMBER_PLAN_ISDN, 1).
-define(NUMBER_LAND_MOBILE, 6).

-define(TRACE, false).

-record(loop_dat, {
	 scrc_pid,
	 m3ua_pid,
     ss7links_pid,
     ss7routes_pid,
     link,
     local_pc,
     remote_pc,
     gt_local,
     gt_hlr,
     gt_vlr,
     gt_msc,
     gt_sgsn,
     gt_gmsc,
     gt_smsc,
     msisdn,
     imsi,
     scenter,
     fnumber
	}).
