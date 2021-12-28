#!/usr/bin/python3
import telnetlib
import socketserver
import socket
import selectors
import json
import re
import sqlite3
import heapq
import sys
import argparse
import logging

GMCP = bytes([201])

sw_cmd = re.compile("^(?:speedwalk(?: +from +(?P<origin>.+?))? to|sw)" +
                    " +(?P<destination>.+?)\r\n$")
shop_cmd = re.compile("^shop (?P<item>.+?)\r\n$")
choice_cmd = re.compile("^(?P<choice>\d+?)\r\n$")

map_names = [
    "Ankh-Morpork", "AM Assassins", "AM Buildings", "AM Cruets", "AM Docks"
    , "AM Guilds", "AM Isle of Gods", "Shades Maze", "Temple of Small Gods"
    , "AM Temples", "AM Thieves", "Unseen University", "AM Warriors"
    , "Pseudopolis Watch House", "Magpyr's Castle", "Bois", "Bes Pelargic"
    , "BP Buildings", "BP Estates", "BP Wizards", "Brown Islands"
    , "Death's Domain", "Djelibeybi", "IIL - DJB Wizards", "Ephebe"
    , "Ephebe Underdocks", "Genua", "Genua Sewers", "GRFLX Caves"
    , "Hashishim Caves", "Klatch Region", "Mano Rossa", "Monks of Cool"
    , "Netherworld", "Pumpkin Town", "Ramtops Regions", "Sto Lat"
    , "Academy of Artificers", "Cabbage Warehouse", "AoA Library"
    , "Sto Lat Sewers", "Sprite Caves", "Sto Plains Region"
    , "Uberwald Region", "UU Library", "Klatchian Farmsteads", "CFT Arena"
    , "PK Arena", "AM Post Office", "Ninja Guild", "The Travelling Shop"
    , "Slippery Hollow", "House of Magic - Creel", "Special Areas"
    , "Skund Wolf Trail"
]

rooms = dict()
exits = dict()

def map_name(map_id):
    if map_id < len(map_names):
        return map_names[map_id]
    else:
        return "Unknown"

def composeGMCPCommand(cmd, data):
    return telnetlib.IAC + telnetlib.SB + GMCP + \
        bytes(cmd + ' ' + json.dumps(data), 'ascii') + \
        telnetlib.IAC + telnetlib.SE

class TelnetHandler(socketserver.BaseRequestHandler):
    def option_cb(self, sock, c, opt):
        """Keeps track of GMCP data."""
        if c == telnetlib.WILL and opt == GMCP:
            logging.debug("Enabling GMCP")
            sock.sendall(telnetlib.IAC + telnetlib.DO + GMCP +
                         composeGMCPCommand("core.hello",
                                            { "client" : "pydwproxy",
                                              "version" : "0" }) +
                         composeGMCPCommand("core.supports.set",
                                            [ "room.info" ]))
        else:
            if opt == telnetlib.NOOPT:
                opt = b''
            if c == telnetlib.SE:
                sb_data = self.tn.read_sb_data()
                if sb_data.startswith(GMCP):
                    gmcp_cmd, gmcp_sep, gmcp_json = \
                        sb_data.removeprefix(GMCP).partition(b' ')
                    self.gmcp_data[gmcp_cmd.decode('ascii')] = \
                        json.loads(gmcp_json)
                    logging.debug("GMCP command received: %s %s",
                                  gmcp_cmd.decode('ascii'),
                                  json.loads(gmcp_json))
                self.request.sendall(sb_data)
            self.request.sendall(telnetlib.IAC + c + opt)

    def process_cmd(self, s):
        """Processes client commands."""

        # Return at once if we don't have room information
        if not self.gmcp_data['room.info']['identifier']:
            return False

        cur = self.server.dbcur

        cur_room_id = self.gmcp_data['room.info']['identifier']
        origin_room = cur_room_id

        sw_match = sw_cmd.match(s)
        shop_match = shop_cmd.match(s)
        choice_match = choice_cmd.match(s)

        if sw_match:
            if sw_match.group('origin'):
                origin_room = sw_match.group('origin')
            dest_room = sw_match.group('destination')
            from_rooms = \
                cur.execute("select room_id from rooms where room_short like ? or room_id = ?",
                            ("%%%s%%" % origin_room, origin_room)).fetchall()
            to_rooms = \
                cur.execute("select room_id from rooms where room_short like ? or room_id = ?",
                            ("%%%s%%" % dest_room, dest_room)).fetchall()
            # Prepare a list of (from, to, path) tuples, sorted by
            # route length.
            routes = list()
            for from_room in from_rooms:
                routes += \
                    map(lambda x: (from_room[0], x[0], x[1]),
                        find_paths(from_room[0],
                                   set(map(lambda x: x[0], to_rooms))).items())
            routes.sort(key=lambda x: len(x[2]))
            if len(routes) == 0:
                self.request.sendall("No routes found\r\n".encode())
            elif len(routes) == 1:
                route = routes[0][2]
                self.request.sendall("One route found, {} steps.\r\n".
                                     format(len(route)).encode())
                self.tn.write("alias _speedwalk {}\r\n_speedwalk\r\n".
                              format(";".join(route)).encode())
            else:
                self.route_choices = list(map(lambda x: x[2], routes))
                self.request.sendall("Multiple routes found:\r\n".encode())
                for i in range(len(routes)):
                    rfrom, rto, rpath = routes[i]
                    if rfrom != cur_room_id:
                        self.request.sendall("From {} ({})\r\n".
                                             format(rooms[rfrom][3],
                                                    map_name(rooms[rfrom][0])).
                                             encode())
                    self.request.sendall("[{}] {} ({}): {} steps\r\n".
                                         format(i, rooms[rto][3],
                                                map_name(rooms[rto][0]),
                                                len(rpath)).
                                         encode())
        elif shop_match:
            item_name = shop_match.group('item')
            shops = \
                cur.execute("select room_id, item_name, sale_price " +
                            "from shop_items " +
                            "natural inner join rooms where item_name like ?",
                            ("%%%s%%" % item_name,)).fetchall()
            if len(shops) == 0:
                self.request.sendall("No shops found\r\n".encode())
            else:
                routes = find_paths(cur_room_id,
                                    set(map(lambda x: x[0], shops)))
                self.route_choices = list()
                for shop in shops:
                    room = rooms[shop[0]]
                    if shop[0] in routes:
                        choice_str = str(len(self.route_choices))
                        route_str = \
                            ", {} steps away".format(len(routes[shop[0]]))
                        self.route_choices.append(routes[shop[0]])
                    else:
                        choice_str = " "
                        route_str = ""
                    shop_str = "[{}] {} ({}{}): {} for {}\r\n".\
                        format(choice_str, room[3], map_name(room[0]),
                               route_str, shop[1], shop[2])
                    self.request.sendall(shop_str.encode())
        elif choice_match:
            choice = int(choice_match.group('choice'))
            if self.route_choices and len(self.route_choices) > choice:
                route = self.route_choices[choice]
                self.tn.write("alias _speedwalk {}\r\n_speedwalk\r\n".
                              format(";".join(route)).encode())
            else:
                self.request.sendall("No such route option found\r\n".encode())
        else:
            return False
        return True

    def handle(self):
        """Client connection handling."""
        logging.info("A new connection from %s", self.client_address)
        self.gmcp_data = {"room.info": {"identifier": None}}
        try:
            with telnetlib.Telnet(self.server.args.host,
                                  self.server.args.port) as tn:
                self.tn = tn
                tn.set_option_negotiation_callback(self.option_cb)
                with selectors.DefaultSelector() as sel:
                    sel.register(self.request, selectors.EVENT_READ)
                    sel.register(tn, selectors.EVENT_READ)
                    connected = True
                    while connected:
                        events = sel.select()
                        for key, mask in events:
                            if key.fileobj is self.request:
                                data = self.request.recv(4096)
                                if data == b'':
                                    tn.close()
                                    connected = False
                                    break
                                else:
                                    try:
                                        input_line = data.decode("utf-8")
                                        if not self.process_cmd(input_line):
                                            tn.get_socket().sendall(data)
                                    except UnicodeDecodeError:
                                        tn.get_socket().sendall(data)
                            elif key.fileobj is tn:
                                try:
                                    data = tn.read_very_eager()
                                    self.request.sendall(data)
                                except EOFError:
                                    connected = False
                                    break
                            else:
                                raise Exception("Unknown fileobj in an event")
        finally:
            logging.info("Shutting down the %s connection", self.client_address)
            self.request.shutdown(socket.SHUT_RDWR)
            self.request.close()


def find_paths(source, destinations):
    """Dijkstra's algorithm with heap queue and multiple destinations"""
    queue = list()
    heapq.heapify(queue)
    found = set()
    queue_entries = dict()
    dist = dict()
    prev = dict()
    for k in exits:
        dist[k] = sys.maxsize
        prev[k] = None
        entry = [dist[k], k]
        if k == source:
            entry = [0, k]
        queue_entries[k] = entry
        heapq.heappush(queue, entry)
    dist[source] = 0
    while queue:
        cur = 'removed'
        while cur == 'removed' and queue:
            priority, cur = heapq.heappop(queue)
            if cur in queue_entries:
                del queue_entries[cur]
        if not queue:
            break
        if cur in destinations:
            found.add(cur)
            if found == destinations:
                break
        for (exit_to, exit_name) in exits[cur]:
            if exit_to in queue_entries:
                if dist[cur] + 1 < dist[exit_to]:
                    dist[exit_to] = dist[cur] + 1
                    prev[exit_to] = (cur, exit_name)
                    # Replace the entry: remove it, then add again
                    entry = queue_entries.pop(exit_to)
                    entry[-1] = 'removed'
                    entry = [dist[exit_to], exit_to]
                    queue_entries[exit_to] = entry
                    heapq.heappush(queue, entry)
    results = dict()
    for d in destinations:
        if prev[d]:
            results[d] = list()
            p = prev[d]
            while p:
                results[d].append(p[1])
                p = prev[p[0]]
            results[d].reverse()
    return results

if __name__ == '__main__':
    con = None
    try:
        parser = argparse.ArgumentParser(description='Run the DW MUD proxy.')
        parser.add_argument('dbfile', metavar='DB_FILE', type=str, nargs='?',
                            help="a path to the Quow's map database",
                            default="_quowmap_database.db")
        parser.add_argument('host', metavar='HOST', type=str, nargs='?',
                            help="remote host",
                            default="discworld.starturtle.net")
        parser.add_argument('port', metavar='PORT', type=int, nargs='?',
                            help="remote port", default=23)
        parser.add_argument('--local-host', metavar='HOST', type=str,
                            help="local host", default="localhost")
        parser.add_argument('--local-port', metavar='PORT', type=int,
                            help="local port", default=2000)
        parser.add_argument('--log-level', metavar='LEVEL', type=int,
                            help="log level", default=logging.INFO)
        args = parser.parse_args()
        logging.basicConfig(level=args.log_level)
        con = sqlite3.connect(args.dbfile)
        cur = con.cursor()
        for row in cur.execute("select room_id, map_id, xpos, ypos, room_short, room_type from rooms"):
            rooms[row[0]] = row[1:]
            exits[row[0]] = []
        for row in cur.execute("select room_id, connect_id, exit from room_exits"):
            exits[row[0]].append((row[1], row[2]))
        with socketserver.TCPServer((args.local_host, args.local_port),
                                    TelnetHandler) as server:
            server.args = args
            server.dbcur = cur
            logging.info('Listening on %s:%d', args.local_host, args.local_port)
            server.serve_forever()
    finally:
        if con:
            con.close()
