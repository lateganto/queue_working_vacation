//
// This file is part of an OMNeT++/OMNEST simulation example.
//
// Copyright (C) 2006-2015 OpenSim Ltd
//
// This file is distributed WITHOUT ANY WARRANTY. See the file
// `license' for details on this and other legal matters.
//

@namespace(queueing);

//
// This simple queueing network only contains a source, a FIFO queue and a sink.
//
network BatchQueueWV
{
    parameters:
        @display("i=block/network2;bgb=485,298");
    submodules:
        sink: Sink {
            @display("p=435,205");
        }
        queue: PassiveQueue {
            @display("p=215,79");
        }
        server: Server {

            @display("p=359,79");
        }
        source: Source {
            @display("p=50.0,79.0");
        }
    connections:
        source.out --> queue.in++;
        queue.out++ --> server.in++;
        server.out --> sink.in++;
}
