//
// This file is part of an OMNeT++/OMNEST simulation example.
//
// Copyright (C) 2006-2015 OpenSim Ltd.
//
// This file is distributed WITHOUT ANY WARRANTY. See the file
// `license' for details on this and other legal matters.
//

#include "Sink.h"
#include "Job.h"

namespace queueing {

	Define_Module(Sink);

	void Sink::initialize() {
		lifeTimeSignal = registerSignal("lifeTime");
		totalQueueingTimeSignal = registerSignal("totalQueueingTime");
		queuesVisitedSignal = registerSignal("queuesVisited");
		totalServiceTimeSignal = registerSignal("totalServiceTime");
		totalDelayTimeSignal = registerSignal("totalDelayTime");
		delaysVisitedSignal = registerSignal("delaysVisited");
		generationSignal = registerSignal("generation");
		keepJobs = par("keepJobs");

		//ADDED
		sojournTimeSignal = registerSignal("sojournTime");
		sojournTimeSignalVacation = registerSignal("sojournTimeVacation");
		sojournTimeSignalBusy = registerSignal("sojournTimeBusy");
	}

	void Sink::handleMessage(cMessage *msg) {
		Job *job = check_and_cast<Job *>(msg);

		// gather statistics
		emit(lifeTimeSignal, simTime() - job->getCreationTime());
		emit(totalQueueingTimeSignal, job->getTotalQueueingTime());
		emit(queuesVisitedSignal, job->getQueueCount());
		emit(totalServiceTimeSignal, job->getTotalServiceTime());
		emit(totalDelayTimeSignal, job->getTotalDelayTime());
		emit(delaysVisitedSignal, job->getDelayCount());
		emit(generationSignal, job->getGeneration());

		////////////////////ADDED////////////////////
		simtime_t d = job->getTotalQueueingTime() + job->getTotalServiceTime();
		emit(sojournTimeSignal, d);
		if(job->getProcessedVacation()) {
			emit(sojournTimeSignalVacation, d);
		} else {
			emit(sojournTimeSignalBusy, d);
		}
		////////////////////ADDED////////////////////

		if (!keepJobs) delete msg;
	}

	void Sink::finish() {
		// TODO missing scalar statistics
	}

}
;
//namespace

