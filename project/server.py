#!/usr/local/bin/python3

import sys
import asyncio
import time
import aiohttp
import key
import json

"""
=========================
        Constants
=========================
"""

PORTS = {
        "Goloman": 12229,
        "Hands": 12230,
        "Holiday": 12231,
        "Welsh": 12232,
        "Wilkes": 12233
        }

GRAPH = {
        "Goloman": ["Hands", "Holiday", "Wilkes"],
        "Hands": ["Goloman", "Wilkes"],
        "Holiday": ["Goloman", "Welsh", "Wilkes"],
        "Welsh": ["Holiday"],
        "Wilkes": ["Goloman", "Hands", "Holiday"]
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

class ServerClientProtocol(asyncio.Protocol):
    def __init__(self, name, loop, clients, servers, logs):
        self.loop = loop
        self.name = name
        self.clients = clients
        self.servers = servers
        self.logs = logs
    
    def connection_made(self, transport):
        print("connection_made")
        self.logs.write("connection made\n")
        self.transport = transport

    def connection_lost(self, exc):
        print("connection_lost")
        self.logs.write("connection lost\n")

    def data_received(self, data):
        # Get data from a client
        message = data.decode()
        print("data received: {!r}".format(message))
        self.logs.write("data received: {!r}\n".format(message))

        # Parse the message. Determine what kind of message it is
        args = message.split()
        op = args[0]
     
        # Take action that's appropriate to the instructions
        switch = {
            "IAMAT": self.iamat,        # IAMAT %cli_id %cli_coord %time
            "WHATSAT": self.whatsat,    # WHATSAT %cli_id %radius %distance
            "CLIAT": self.cliat,        # CLIAT %orig_serv %first_serv %cli_id %radius %distance
        }
        func = switch.get(op, self.invalid)
        asyncio.ensure_future(func(args))

    async def iamat(self, args):
        if len(args) != 4:
            return await asyncio.ensure_future(self.invalid(args))

        try:
            arg = Client(args[1:], self.name)
        except ValueError:
            return await asyncio.ensure_future(self.invalid(args))

        # Respond to the client
        now = time.time()
        time_diff = now - arg.time
        try:
            arg.set_time_diff(time_diff)
        except ValueError:
            return await asyncio.ensure_future(self.invalid(args))
        reply = "AT {} {} {}".format(self.name, time_diff, arg.string())

        print("iamat:\t\tsend: {!r}".format(reply))
        self.transport.write(reply.encode())
        self.logs.write("data sent to client: {!r}\n".format(reply))

        # check if a newer location exists for the client
        # If not, propagate the new location to neighbouring servers
        if check_newer(self.clients, arg.id, arg.time):
            print("iamat:\t\tUpdating client {}".format(arg.id))
            self.clients[arg.id] = arg

            print("iamat:\t\tPropagating client {} information".format(arg.id))
            asyncio.ensure_future(self.propagate(arg, None))

    async def propagate(self, arg, excl):
        for neighbour in GRAPH.get(self.name, []):
            if neighbour != excl:
                # first check if connection is established with all the neighbouring servers
                # if not, establish a connection
                if self.servers.get(neighbour) == None:
                    coro = self.loop.create_connection(
                            lambda: ServerServerProtocol(neighbour, self.clients, self.servers, self.logs),
                            '127.0.0.1', PORTS.get(neighbour))
                    try:
                        self.servers[neighbour] = await asyncio.ensure_future(coro)
                    except ConnectionRefusedError:
                        print("Can't connect to {}".format(neighbour))
                        continue

                #send a cliat message
                message = "CLIAT {} {} {} {}".format(self.name, arg.server, arg.time_diff, arg.string())
                (self.servers[neighbour])[0].write(message.encode())
                print('Client:\t\tData sent: {!r}'.format(message))
                self.logs.write("data sent to {}: {!r}\n".format(neighbour, message))

    async def cliat(self, args):
        if len(args) != 7:
            return await asyncio.ensure_future(self.invalid(args))

        print("cliat:\t\tKnown Clients:{}".format(self.clients))
        sender = args[1]
        orig_serv = args[2]
        if (not sender in PORTS) or (not orig_serv in PORTS):
            return await asyncio.ensure_future(self.invalid(args))

        time_diff = args[3]
        try:
            arg = Client(args[4:], orig_serv)
            arg.set_time_diff(time_diff)
        except ValueError:
            return await asyncio.ensure_future(self.invalid(args))

        # check if a newer location exists for the client
        # If not, propagate the new location to neighbouring servers
        if check_newer(self.clients, arg.id, arg.time):
            print("cliat:\t\tUpdating client {}".format(arg.id))
            self.clients[arg.id] = arg
            print("cliat:\t\tPropagating client {} information".format(arg.id))
            asyncio.ensure_future(self.propagate(arg, sender))
        else:
            print("cliat:\t\tNo new clients")

    async def whatsat(self, args):
        if len(args) != 4:
            return await asyncio.ensure_future(self.invalid(args))

        # parse args
        # WHATSAT $CLI_ID $RADIUS $AMT_INFO
        client = self.clients.get(args[1])
        if client == None:
            return await asyncio.ensure_future(self.invalid(args))

        try:
            radius = float(args[2])
            amt_info = int(args[3])
        except ValueError:
            return await asyncio.ensure_future(self.invalid(args))

        location = fix_coord(client.coord)
        radius_int = int(1000*radius)

        url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={}&radius={}&key={}\n".format(
                location, radius_int, key.KEY)

        """
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as resp:
                json_response = await resp.text()
                response = json.loads(json_response)

                if response['status'] == "OK":
                    if len(response['results']) > amt_info:
                        response['results'] = response['results'][:amt_info]

                msg = json.dumps(response)

                reply = "AT {} {} {}\n{}\n\n".format(client.server, client.time_diff, client.string(), msg)

                print("whatsat:\t\tsend: {!r}".format(reply))
                self.transport.write(reply.encode())
                self.logs.write("data sent to client: {!r}\n".format(reply))
        """

        print("whatsat:\t\tsend: {!r}".format(url))
        self.transport.write(url.encode())
        self.logs.write("data sent to client: {!r}\n".format(url))

       
    async def invalid(self, args):
        msg = "? {}".format(" ".join(args))
        print("invalid:\t\tsend: {!r}".format(msg))
        self.transport.write(msg.encode())
        self.logs.write("data sent: {!r}\n".format(msg))


# Represents the protocols when a server is contacting another server, acting as a client
class ServerServerProtocol(asyncio.Protocol):
    def __init__(self, partner, clients, servers, logs):
        self.clients = clients
        self.partner = partner 
        self.servers = servers
        self.logs = logs

    def connection_made(self, transport):
        # assign transport to itself
        self.transport = transport

        #logs
        print("Connection made with {}".format(self.partner))
        self.logs.write("connection made with {}\n".format(self.partner))


    # Server to server communication.
    #   Servers only communicate about propagating client information
    def data_received(self, data):

        msg = data.decode()
        print("Data received from {}: {}".format(self.parter, msg))
        self.logs.write("data received from {}: {!r}\n".format(self.partner, msg))

        args = msg.split()
        op = args[0]

        switch = {
            "CLIAT": self.cliat     # CLIAT %orig_server %cli_id %cli_coord %time 
        }
        func = switch.get(op, self.invalid)
        asyncio.ensure_future(func(args))


    async def cliat(self, args):
        if len(args) != 7:
            return await asyncio.ensure_future(self.invalid(args))

        print("cliat:\t\tKnown Clients:{}".format(self.clients))
        sender = args[1]
        orig_serv = args[2]
        if (not sender in PORTS) or (not orig_serv in PORTS):
            return await asyncio.ensure_future(self.invalid(args))

        time_diff = args[3]
        try:
            arg = Client(args[4:], orig_serv)
            arg.set_time_diff(time_diff)
        except ValueError:
            return await asyncio.ensure_future(self.invalid(args))

        # check if a newer location exists for the client
        # If not, propagate the new location to neighbouring servers
        if check_newer(self.clients, arg.id, arg.time):
            print("cliat:\t\tUpdating client {}".format(arg.id))
            self.clients[arg.id] = arg
            print("cliat:\t\tPropagating client {} information".format(arg.id))
            asyncio.ensure_future(self.propagate(arg, sender))
        else:
            print("cliat:\t\tNo new clients")
     
    async def propagate(self, arg, excl):
        for neighbour in GRAPH.get(self.name, []):
            if neighbour != excl:
                # first check if connection is established with all the neighbouring servers
                # if not, establish a connection
                if self.servers.get(neighbour) == None:
                    coro = self.loop.create_connection(
                            lambda: ServerServerProtocol(neighbour, self.clients, self.servers, self.logs),
                            '127.0.0.1', PORTS.get(neighbour))
                    try:
                        self.servers[neighbour] = await asyncio.ensure_future(coro)
                    except ConnectionRefusedError:
                        print("Can't connect oh no")
                        continue

                #send a cliat message
                message = "CLIAT {} {} {} {}".format(self.name, arg.server, arg.time_diff, arg.string())
                self.transport.write(message.encode())
                print('Client:\t\tData sent: {!r}'.format(message))
                self.logs.write("data sent to {}: {!r}\n".format(self.partner, message))

    def connection_lost(self, exc):
        # report corresponding logs
        print("Connection lost with {}".format(self.partner))
        self.logs.write("connection lost with {}\n".format(self.partner))

    async def invalid(self, args):
        msg = "? {}".format(" ".join(args))
        print("invalid:\t\tsend: {!r}".format(msg))
        self.transport.write(msg.encode())
        self.logs.write("data sent to {}: {!r}\n".format(self.partner, msg))

class Client():
    def __init__(self, args, server):
        self.id = args[0]
        self.coord = args[1]
        self.time = float(args[2])
        self.server = server

    def string(self):
        return "{} {} {}".format(self.id, self.coord, self.time)

    def set_time_diff(self, time_diff):
        self.time_diff = float(time_diff)

"""
=================================
        Supporting Cast
=================================
"""

def fix_coord(coord):
    spr_ind = max(coord.find('+'), coord.find('-'))
    with_comma = "".join([coord[:spr_ind], ",", coord[spr_ind:]])
    return "".join(with_comma.split("+"))

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

    server_name = sys.argv[1]

    print(server_name)

    logs = open("{}_logs.txt".format(server_name), "w")

    mob_clis = {}
    servers = {}
    coro = loop.create_server(lambda: ServerClientProtocol(server_name, loop, mob_clis, servers, logs),
            '127.0.0.1', PORTS.get(server_name, 9000))
    server = loop.run_until_complete(coro)

    #print("serving on {}".format(server.sockets[0].getsockname()))
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()
    logs.close()


if __name__ == "__main__":
    main()
