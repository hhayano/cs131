#!/usr/local/bin/python3

import sys
import asyncio
import time

"""
=========================
        Constants
=========================
"""

PORTS = {
        "Goloman": 12229,
        "Hands": 12230,
#        "Holiday": 12231,
#        "Welsh": 12232,
#        "Wilkes": 12233
        }

GRAPH = {
#        "Goloman": ["Hands", "Holiday", "Wilkes"],
        "Goloman": ["Hands"],
        "Hands": ["Goloman"]
#        "Hands": ["Goloman", "Wilkes"],
#        "Holiday": ["Goloman", "Welsh", "Wilkes"],
#        "Welsh": ["Holiday"],
#        "Wilkes": ["Goloman", "Hands", "Holiday"]
        }

"""
==============================
        Test Functions
==============================
"""

async def lazy_range(up_to):
    index = 0
    while index < up_to:
        index = await increment(index)
        print(index)

async def increment(x):
    return x+1


"""
=================================
        Class Definitions
=================================
"""

class ServerProtocol(asyncio.Protocol):

    def __init__(self, name, loop, clients):
        print("new server")
        self.loop = loop
        self.name = name
        self.clients = clients
    
    def connection_made(self, transport):
        print("connection_made")
        self.transport = transport

    def connection_lost(self, exc):
        print("connection_lost")

    def data_received(self, data):

        # Get data from unknown source, could be another server or a client
        message = data.decode()
        print("data received: {!r}".format(message))

        # Parse the message. Determine what kind of message it is
        op_list = message.split()
        op = op_list[0]
        args = op_list[1:]
     
        # Take action that's appropriate to the instructions
        switch = {
            "IAMAT": self.iamat,
            "WHATSAT": self.whatsat,
            "CLIAT": self.cliat
        }
        func = switch.get(op, self.invalid)
        asyncio.ensure_future(func(args))
     

    async def iamat(self, args):
        arg = Client(args)

        # First check if a newer location exists for the client
        # If not, propagate the new location to neighbouring servers
        if check_newer(self.clients, arg.id, arg.time):
            print("iamat:\t\tUpdating client {}".format(arg.id))
            self.clients[arg.id] = arg
            print("iamat:\t\tPropagating client {} information".format(arg.id))
            self.propagate(arg, None)

        # Respond to the client
        now = time.time()
        time_diff = now - arg.time
        reply = "AT {} {} {}".format(self.name, time_diff, arg.string())

        print("iamat:\t\tsend: {!r}".format(reply))
        self.transport.write(reply.encode())

    def propagate(self, arg, excl):
        for neighbour in GRAPH.get(self.name, []):
            if neighbour != excl:
                coro = self.loop.create_connection(
                        lambda: ClientProtocol(self.name, arg),
                        '127.0.0.1', PORTS.get(neighbour))
                asyncio.ensure_future(coro)

    async def whatsat(self, arg):
        print("whatsat")

    async def cliat(self, args):
        print("cliat:\t\tKnown Clients:{}".format(self.clients))
        sender = args[0]
        arg = Client(args[1:])

        # check if a newer location exists for the client
        # If not, propagate the new location to neighbouring servers
        if check_newer(self.clients, arg.id, arg.time):
            print("cliat:\t\tUpdating client {}".format(arg.id))
            self.clients[arg.id] = arg
            print("cliat:\t\tPropagating client {} information".format(arg.id))
            self.propagate(arg, sender)
        else:
            print("cliat:\t\tNo new clients")
        
    async def invalid(self, arg):
        return "? {}".format(arg.string())


# Represents the protocols when a server is contacting another server, acting as a client
class ClientProtocol(asyncio.Protocol):
    def __init__(self, name, client):
        self.client = client
        self.name = name

    def connection_made(self, transport):
        message = "CLIAT {} {}".format(self.name, self.client.string())
        transport.write(message.encode())
        print('Client:\t\tData sent: {!r}'.format(message))
        print("Client:\t\tClosing connection~")
        transport.close()

    def connection_lost(self, exc):
        print("Client:\t\tconnection_lost")



class Client():
    def __init__(self, args):
        self.id = args[0]
        self.coord = args[1]
        self.time = float(args[2])

    def string(self):
        return "{} {} {}".format(self.id, self.coord, str(self.time))

"""
=================================
        Supporting Cast
=================================
"""

def check_newer(clis, new_cli, time):
    entry = clis.get(new_cli)
    if entry == None:
        return True

    if time > entry.time:
        print("check_newer:\t\tOld time: {}\nNew time: {}".format(entry.time, time))
        return True
    else:
        return False


def main():

    # Create event loop
    loop = asyncio.get_event_loop()

    print(sys.argv[1])

    mob_clis = {}
    coro = loop.create_server(lambda: ServerProtocol(sys.argv[1], loop, mob_clis),
            '127.0.0.1', PORTS.get(sys.argv[1], 9000))
    server = loop.run_until_complete(coro)

    #print("serving on {}".format(server.sockets[0].getsockname()))
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()


if __name__ == "__main__":
    main()
