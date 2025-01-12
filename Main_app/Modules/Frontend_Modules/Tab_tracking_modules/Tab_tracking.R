## Tab completion tracking

Tab_Completion_tracking <- rep(0, length(TABS_CHANGE_STATE))
names(Tab_Completion_tracking) <- TABS_CHANGE_STATE
Tab_Completion_tracking <- reactiveValues(state = Tab_Completion_tracking)