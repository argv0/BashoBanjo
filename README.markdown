# What is Basho Banjo?

Basho Banjo is an experiment in using Riak Core to create a
distributed orchistra powered by midi files. It serves as a simple
example of how to create a distributed application with Riak Core.

# How Does it Work?

To understand 'Banjo, you first need to understand a little about Riak
Core. Riak Core is an open-source Erlang library that allows you to
create masterless distributed applications using the principles
described in Amazon's Dynamo Paper. For more information about Riak
Core, you can view [this
presentation](http://www.slideshare.net/rklophaus/masterless-distributed-computing-with-riak-core-euc-2010).

For our purposes, there are two main things to consider when creating
an application on Riak Core:

1. *Commands* are the operations that you send to the system, and
consist of an ObjectName and a Payload. Banjo commands consist of a
command to play a note of a certain frequency/volume/duration.

2. The *VNode Module* is a pluggable module that conforms to an interface
provided by Riak Core that accepts incoming commands and acts on
them. The Banjo VNode accepts the incoming command to play a note,
generates a wave file (if it doesn't already exist) and then plays the
file using aplay or afplay, depending on the platform. (Mac and Linux are
supported... Windows is not.)

With these two things implemented, Riak Core does the rest of the
work. It uses a consistent hash to route notes to specific VNodes, and
it takes care of assigning VNodes (a.k.a. Virtual Nodes) to physical
nodes, and re-assigning the VNodes when you add/remove physical
nodes to/from the cluster.

The last part of the equation is some code that uses an [Erlang midi
library](https://github.com/jimm/erlang-midilib) developed by Jim
Menard to parse midi files and extract notes. The code than iterates
through the notes with the correct timing, generating play commands to
send to Riak Core.

# How Do I Run Basho Banjo?

### Get the Code and Compile

    git clone https://github.com/rklophaus/BashoBanjo.git
    cd BashoBanjo
    make rel

### Start Basho Banjo

    cd rel/basho_banjo
    bin/banjo console

### Play a Note (from the Erlang console)
     
    play(78).

### Play a Midi Tune

    play("../../midi/mario.mid").
    play("../../midi/zelda.mid").

### Connect to Another Banjo Node

If you want to run multiple nodes on a single computer, you can:

1. Copy the files in `rel/basho_banjo` to another directory.
2. Edit `etc/vm.args` and change the node name to something like `banjo1@127.0.0.`
3. Edit `etc/app.config` and change the handoff port to avoid a port conflict.
4. Start the new node.
5. Join the new node to the existing node from the Erlang console: `join('banjo1@hostname').`
6. Then, play a midi tune, which should play across both nodes: `play("../../midi/mario.mid").`