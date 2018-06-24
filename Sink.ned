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
// Destroys (or optionally keep) the packets and collects statistics
//
simple Sink
{
    parameters:
        @group(Queueing);
        @display("i=block/sink");
        @signal[lifeTime](type="simtime_t");
        @signal[totalQueueingTime](type="simtime_t");
        @signal[totalDelayTime](type="simtime_t");
        @signal[totalServiceTime](type="simtime_t");
        @signal[queuesVisited](type="long");
        @signal[delaysVisited](type="long");
        @signal[generation](type="long");
        
        ////////////////////ADDED////////////////////
       	@signal[sojournTime](type="simtime_t");				//sojourn time of the job (queueing time + service time)
       	@signal[sojournTimeVacation](type="simtime_t");		//sojourn time of the job (queueing time + service time) for job serviced during vacation
       	@signal[sojournTimeBusy](type="simtime_t");			//sojourn time of the job (queueing time + service time) for job serviced during busy period
       	
       	@signal[queueingTimeBusy](type="simtime_t");		
       	@signal[queueingTimeVacation](type="simtime_t");
       	@signal[serviceTimeBusy](type="simtime_t");	
        @signal[serviceTimeVacation](type="simtime_t");
        ////////////////////ADDED////////////////////
       
        @statistic[lifeTime](title="lifetime of arrived jobs"; unit=s; record=vector?,mean,max; interpolationmode=none);
        @statistic[totalQueueingTime](title="the total time spent in queues by arrived jobs"; unit=s; record=vector?,mean,max; interpolationmode=none);
        @statistic[totalDelayTime](title="the total time spent in delay nodes by arrived jobs"; unit=s; record=vector?,mean,max; interpolationmode=none);
        @statistic[totalServiceTime](title="the total time spent  by arrived jobs"; unit=s; record=vector?,mean,max; interpolationmode=none);
        @statistic[queuesVisited](title="the total number of queues visited by arrived jobs"; record=vector?,mean,max; interpolationmode=none);
        @statistic[delaysVisited](title="the total number of delays visited by arrived jobs"; record=vector?,mean,max; interpolationmode=none);
        @statistic[generation](title="the generation of the arrived jobs"; record=vector?,mean,max; interpolationmode=none);
        
        ////////////////////ADDED////////////////////
        @statistic[sojournTime](title="the total time spent in queues and service by arrived jobs"; unit=s; record=vector,stats; interpolationmode=none);
        @statistic[sojournTimeVacation](title="the total time spent in queues and service by arrived jobs during vacation period"; unit=s; record=vector,stats; interpolationmode=none);
        @statistic[sojournTimeBusy](title="the total time spent in queues and service by arrived jobs during busy period"; unit=s; record=vector,stats; interpolationmode=none);
        
        @statistic[queueingTimeBusy](title="the time spent in queue by arrived jobs during busy period"; unit=s; record=vector,stats; interpolationmode=none);
        @statistic[queueingTimeVacation](title="the time spent in queue by arrived jobs during vacation period"; unit=s; record=vector,stats; interpolationmode=none);
        @statistic[serviceTimeBusy](title="the time spent in service by arrived jobs during busy period"; unit=s; record=vector,stats; interpolationmode=none);
        @statistic[serviceTimeVacation](title="the time spent in service by arrived jobs during vacation period"; unit=s; record=vector,stats; interpolationmode=none);
        ////////////////////ADDED////////////////////
        
        bool keepJobs = default(false); // whether to keep the received jobs till the end of simulation
    gates:
        input in[];
}

