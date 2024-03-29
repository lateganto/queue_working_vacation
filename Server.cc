//
// This file is part of an OMNeT++/OMNEST simulation example.
//
// Copyright (C) 2006-2015 OpenSim Ltd.
//
// This file is distributed WITHOUT ANY WARRANTY. See the file
// `license' for details on this and other legal matters.
//

#include "Server.h"
#include "Job.h"
#include "SelectionStrategies.h"
#include "IPassiveQueue.h"

namespace queueing {

	Define_Module(Server);

	Server::Server() {
		selectionStrategy = nullptr;
		jobServiced = nullptr;
		endServiceMsg = nullptr;
		allocated = false;
		vacation = false;
	}

	Server::~Server() {
		delete selectionStrategy;
		delete jobServiced;
		cancelAndDelete(endServiceMsg);
	}

	void Server::initialize() {
		busySignal = registerSignal("busy");
		emit(busySignal, false);

		////////////////////ADDED////////////////////
		vacationPeriodSignal = registerSignal("vacationPeriod"); //emitted when the server goes in vacation
		emit(vacationPeriodSignal, false);
		endVacationMsg = new cMessage("end-vacation");
		////////////////////ADDED////////////////////

		endServiceMsg = new cMessage("end-service");
		jobServiced = nullptr;
		allocated = false;
		selectionStrategy = SelectionStrategy::create(par("fetchingAlgorithm"), this, true);
		if (!selectionStrategy) throw cRuntimeError("invalid selection strategy");
	}

	void Server::handleMessage(cMessage *msg) {
		if (msg == endServiceMsg) {  //server has processed a job

			////////////////////ADDED////////////////////
			//when the server finished to serve a Job checks if the queue is empty
			cGate *gate = selectionStrategy->selectableGate(0); //select output gate of the module "Queue"
			if (check_and_cast<IPassiveQueue *>(gate->getOwnerModule())->length() == 0) {  //if the queue is empty server starts vacation
				if (!vacation) { //if the server is already in vacation do not start another one
					EV << "The server is running in vacation mode!\n";
					bubble("Vacation");
					vacation = true;
					emit(vacationPeriodSignal, true);
					scheduleAt(simTime() + par("vacationPeriod").doubleValue(), endVacationMsg);
				}
			}
			////////////////////ADDED////////////////////

			ASSERT(jobServiced != nullptr);
			ASSERT(allocated);
			simtime_t d = simTime() - endServiceMsg->getSendingTime();
			jobServiced->setTotalServiceTime(jobServiced->getTotalServiceTime() + d);
			send(jobServiced, "out");
			jobServiced = nullptr;
			allocated = false;
			emit(busySignal, false);

			// examine all input queues, and request a new job from a non empty queue
			int k = selectionStrategy->select();
			if (k >= 0) {
				EV << "requesting job from queue " << k << endl;
				cGate *gate = selectionStrategy->selectableGate(k);
				check_and_cast<IPassiveQueue *>(gate->getOwnerModule())->request(gate->getIndex());
			}
		}
		////////////////////ADDED////////////////////
		else if (msg == endVacationMsg) { //message of end of the vacation
			cGate *gate = selectionStrategy->selectableGate(0); //select output gate of the module "Queue"
			if (check_and_cast<IPassiveQueue *>(gate->getOwnerModule())->length() == 0) {  //the queue is still empty then return in vacation
				EV << "The server starts another vacation!\n";
				bubble("Another Vacation");
				scheduleAt(simTime() + par("vacationPeriod").doubleValue(), endVacationMsg);
			} else {  //ends the vacation
				EV << "The server is running in normal mode!\n";
				bubble("Normal mode");
				vacation = false;
				emit(vacationPeriodSignal, false);
			}
		////////////////////ADDED////////////////////
		} else {  //arrived a Job
			if (!allocated) error("job arrived, but the sender did not call allocate() previously");
			if (jobServiced) throw cRuntimeError("a new job arrived while already servicing one");

			jobServiced = check_and_cast<Job *>(msg);

			////////////////////ADDED////////////////////
			simtime_t serviceTime;
			if (vacation) {
				jobServiced->setProcessedVacation(true);
				serviceTime = par("serviceTimeVacation");
			} else {
				jobServiced->setProcessedVacation(false);
				serviceTime = par("serviceTimeBusy");
			}
			////////////////////ADDED////////////////////

			scheduleAt(simTime() + serviceTime, endServiceMsg);
			emit(busySignal, true);
		}
	}

	void Server::refreshDisplay() const {
		getDisplayString().setTagArg("i2", 0, jobServiced ? "status/execute" : "");
	}

	void Server::finish() {
	}

	bool Server::isIdle() {
		return !allocated; // we are idle if nobody has allocated us for processing
	}

	////////////////////ADDED////////////////////
	bool Server::isVacation() {
		return vacation;
	}
	////////////////////ADDED////////////////////

	void Server::allocate() {
		allocated = true;
	}

}
;
//namespace

