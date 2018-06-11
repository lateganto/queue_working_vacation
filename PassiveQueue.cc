//
// This file is part of an OMNeT++/OMNEST simulation example.
//
// Copyright (C) 2006-2015 OpenSim Ltd.
//
// This file is distributed WITHOUT ANY WARRANTY. See the file
// `license' for details on this and other legal matters.
//

#include "PassiveQueue.h"
#include "Job.h"
#include "IServer.h"

namespace queueing {

	Define_Module(PassiveQueue);

	PassiveQueue::PassiveQueue() {
		selectionStrategy = nullptr;
	}

	PassiveQueue::~PassiveQueue() {
		delete selectionStrategy;
	}

	void PassiveQueue::initialize() {
		droppedSignal = registerSignal("dropped");
		queueingTimeSignal = registerSignal("queueingTime");
		queueLengthSignal = registerSignal("queueLength");
		emit(queueLengthSignal, 0);

		////////////////////ADDED////////////////////
		queueLengthSignalBusy = registerSignal("queueLengthBusy");
		queueLengthSignalVacation = registerSignal("queueLengthVacation");
		emit(queueLengthSignalBusy, 0);
		emit(queueLengthSignalVacation, 0);
		////////////////////ADDED////////////////////

		capacity = par("capacity");
		queue.setName("queue");

		fifo = par("fifo");

		selectionStrategy = SelectionStrategy::create(par("sendingAlgorithm"), this, false);
		if (!selectionStrategy) throw cRuntimeError("invalid selection strategy");
	}

	void PassiveQueue::handleMessage(cMessage *msg) {
		Job *job = check_and_cast<Job *>(msg);
		job->setTimestamp();  //used to calculate the total queueing time, sets the timestamp to the current simulation time

		// check for container capacity
		if (capacity >= 0 && queue.getLength() >= capacity) {
			EV << "Queue full! Job dropped.\n";
			if (hasGUI()) bubble("Dropped!");
			emit(droppedSignal, 1);
			delete msg;
			return;
		}

		int k = selectionStrategy->select();
		if (k < 0) {
			// enqueue if no idle server found
			queue.insert(job);

			emit(queueLengthSignal, length());

			////////////////////ADDED////////////////////
			cGate *out = gate("out", 0);  //select output gate of the module "PassiveQueue" (that conduct to "Server")
			if (check_and_cast<IServer *>(out->getPathEndGate()->getOwnerModule())->isVacation()) {
				emit(queueLengthSignalVacation, length());
			} else {
				emit(queueLengthSignalBusy, length());
			}
			////////////////////ADDED////////////////////

			job->setQueueCount(job->getQueueCount() + 1);
		} else if (length() == 0) {
			// send through without queueing
			sendJob(job, k);
		} else throw cRuntimeError("This should not happen. Queue is NOT empty and there is an IDLE server attached to us.");
	}

	void PassiveQueue::refreshDisplay() const {
		// change the icon color
		getDisplayString().setTagArg("i", 1, queue.isEmpty() ? "" : "cyan");
	}

	int PassiveQueue::length() {
		return queue.getLength();
	}

	void PassiveQueue::request(int gateIndex) {
		Enter_Method("request()!");

		ASSERT(!queue.isEmpty());

		Job *job;
		if (fifo) {
			job = (Job *)queue.pop();
		}
		else {
			job = (Job *)queue.back();
			// FIXME this may have bad performance as remove uses linear search
			queue.remove(job);
		}

		emit(queueLengthSignal, length());

		////////////////////ADDED////////////////////
		cGate *out = gate("out", gateIndex);
		if (check_and_cast<IServer *>(out->getPathEndGate()->getOwnerModule())->isVacation()) {
			emit(queueLengthSignalVacation, length());
		} else {
			emit(queueLengthSignalBusy, length());
		}
		////////////////////ADDED////////////////////

		job->setQueueCount(job->getQueueCount()+1);
		simtime_t d = simTime() - job->getTimestamp();
		job->setTotalQueueingTime(job->getTotalQueueingTime() + d);
		emit(queueingTimeSignal, d);

		sendJob(job, gateIndex);
	}

	void PassiveQueue::sendJob(Job *job, int gateIndex) {
		cGate *out = gate("out", gateIndex);
		send(job, out);
		check_and_cast<IServer *>(out->getPathEndGate()->getOwnerModule())->allocate();
	}

}
;
//namespace

