//
// This file is part of an OMNeT++/OMNEST simulation example.
//
// Copyright (C) 2006-2015 OpenSim Ltd.
//
// This file is distributed WITHOUT ANY WARRANTY. See the file
// `license' for details on this and other legal matters.
//

@namespace(queueing);

//
// Queue server. It serves multiple input queues (PassiveQueue), using a preset
// algorithm. Inputs must be connected to Passive Queues (PassiveQueue)
//
simple Server
{
    parameters:
        @group(Queueing);
        @display("i=block/server");
        @signal[busy](type="bool");
        @statistic[busy](title="server busy state"; record=vector?,timeavg; interpolationmode=sample-hold);

        string fetchingAlgorithm @enum("priority","random","roundRobin","longestQueue") = default("priority");
             // how the next job will be choosen from the attached queues

        ////////////////////ADDED////////////////////
        @signal[vacationPeriod](type="bool");			//boolean signal to verify when the server goes in vacation
        @statistic[vacationPeriod](title="server vacation state"; record=vector; interpolationmode=sample-hold);
        volatile double serviceTimeBusy @unit(s);		//service time durig normal period  
        volatile double serviceTimeVacation @unit(s);	//service time during vacation period
        volatile double vacationPeriod @unit(s);		//duration of the vacation
        ////////////////////ADDED////////////////////

    gates:
        input in[];
        output out;
}


