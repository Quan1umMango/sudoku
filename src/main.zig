const std = @import("std");
const posix = std.posix;
const EnumSet = std.EnumSet;
const ArrayBitSet = std.bit_set.ArrayBitSet;

const INCORRECT_COLOR = "\x1b[1m\x1b[31m";
const SELECTED_COLOR = "\x1b[1m\x1b[36m";

const BoardError = error{
    InvalidSqaureIndex,
};

pub const PointerPosition = struct {
    const Self = @This();
    x: usize,
    y: usize,

    pub fn toBoardIndex(self: *const Self, boardN: usize) usize {
        const pos = self.y * boardN + self.x;
        std.debug.assert(pos < boardN * boardN);

        return pos;
    }
};

pub const CellState = enum(u8) {
    Selected,
    Incorrect,
    Normal,
};

pub const Cell = struct {
    const Self = @This();

    value: ?u8, // change this to make it take more than u4 (for anything when Board n != 9)
    state: EnumSet(CellState) = EnumSet(CellState).initMany(&[_]CellState{CellState.Normal}),
    possibleValues: [9]bool = [_]bool{ true, true, true, true, true, true, true, true, true },
    canChange: bool,
    // wont really use this, but its to differentiate between two cells if they have other equal properties
    /// index of the cell in the array
    position: usize,

    pub fn setData(self: *Self, d: ?u8) void {
        self.value = d;
    }

    pub fn print(self: *const Self, writer: anytype) !void {
        if (self.state.contains(CellState.Selected)) {
            _ = try writer.write(SELECTED_COLOR);
        } else if (self.state.contains(CellState.Incorrect)) {
            _ = try writer.write(INCORRECT_COLOR);
        }

        if (!self.canChange) {
            _ = try writer.write("\x1b[1;42m");
        }
        // we can only use 1 as no of elements, if n of board <= 9
        // TODO: change it lol
        var buf: [1]u8 = undefined;
        if (self.value == null) {
            buf = "_".*;
        } else {
            _ = try std.fmt.bufPrint(&buf, "{}", .{self.value.?});
        }
        _ = try writer.write(" ");
        _ = try writer.write(&buf);
        // reset the colors
        _ = try writer.write("\x1b[0m");
    }
};

pub const Action = union(enum) {
    // previous pointer position, prior to the move
    move: PointerPosition,

    // data of the cell, prior to the change
    cell: struct { prev: Cell, after: Cell },
};

pub const BoardMode = enum {
    ViewOnly,
    ViewWriteAll,
    // Allows you to edit cells with .canChange equal to true
    ViewWriteSome,
};

pub fn Board(n: usize) type {
    return struct {
        const Self = @This();

        data: [n * n]Cell = [_]Cell{.{ .value = null, .canChange = true, .position = 0 }} ** (n * n),
        nSqrt: usize = std.math.sqrt(n),
        // where the user is currently pointing to. The user will enter the number at this index
        pointer: PointerPosition = .{ .x = 0, .y = 0 },

        // We will use this for undo redo mechanism
        actionsBuf: std.ArrayList(Action),
        actionsPointer: usize = 0,

        mode: BoardMode = .ViewWriteAll,

        pub fn new() Self {
            var s = Self{ .actionsBuf = std.ArrayList(Action).init(std.heap.page_allocator) };
            for (0..s.data.len) |i| {
                s.data[i].position = i;
            }
            return s;
        }

        pub fn deinitActionBuf(self: *Self) void {
            self.actionsBuf.deinit();
        }

        pub fn newFromNumSlice(d: [n * n]?u8) Self {
            var s = Self.new();
            for (d, 0..) |v, i| {
                s.data[i].value = v;
                s.data[i].canChange = (v == null);
            }
            return s;
        }
        pub fn newFrom(d: [n * n]Cell) Self {
            const s = std.ArrayList(Action).init(std.heap.page_allocator);
            return Self{
                .data = d,
                .actionsBuf = s,
            };
        }

        // i could just fetch it from an api, but i think its better to randomly generate it myself for educational purposes
        pub fn newRandom2(random: std.Random) Self {
            var s = Self.new();
            const numHints = random.intRangeAtMost(u8, 25, 30);
            for (0..numHints) |_| {
                var index = random.intRangeAtMost(u8, 0, (n * n) - 1);
                while (s.data[index].value != null) {
                    index = random.intRangeAtMost(u8, 0, (n * n) - 1);
                }
                var notpossible: [n]u8 = undefined;
                _ = s.getGroupUniqueInt(index / n, index % n, &notpossible);
                var possible: [n]u8 = undefined;
                var nPossible: usize = 0;
                {
                    for (1..10) |yeah| {
                        const i: u8 = @intCast(yeah);
                        if (std.mem.containsAtLeastScalar(u8, &notpossible, 1, i)) {
                            continue;
                        }
                        possible[nPossible] = i;
                        nPossible += 1;
                    }
                }
                if (nPossible == 0) {
                    continue;
                }

                const j = random.intRangeAtMost(u8, 0, @as(u8, @intCast(nPossible - 1)));
                const num = possible[j];

                s.data[index].value = num;
            }
            s.setNonNullsAsUnchangeable();
            return s;
        }

        pub fn newRandom() !Self {
            var s = Self.new();
            const Arena = std.heap.ArenaAllocator;
            const page_alloc = std.heap.page_allocator;

            var arena = Arena.init(page_alloc);
            defer arena.deinit();

            var json = std.ArrayList(u8).init(arena.allocator());

            var client = std.http.Client{ .allocator = arena.allocator() };

            var buf: [4060]u8 = undefined;

            const uri = try std.Uri.parse("https://sudoku-api.vercel.app/api/dosuku");
            var req = try client.open(.GET, uri, .{ .server_header_buffer = &buf });

            try req.send();
            try req.finish();

            try req.wait();
            if (req.response.status != .ok) {
                return s;
            }

            _ = try client.fetch(.{ .response_storage = .{ .dynamic = &json }, .location = .{ .uri = uri } });

            const BoardJsonResp = struct {
                newboard: struct {
                    grids: [1]struct { value: [9][9]u8 },
                    //difficulty: [6]u8,
                    //solution: [9][9]u8,
                },
            };

            const parsed = try std.json.parseFromSliceLeaky(BoardJsonResp, arena.allocator(), json.items[0..], .{ .ignore_unknown_fields = true });
            const newB = parsed.newboard.grids[0];
            for (newB.value, 0..) |row, i| {
                for (row, 0..) |d, j| {
                    if (d == 0) {
                        continue;
                    }
                    s.setCellUnlogged(i, j, d);
                }
            }

            s.setNonNullsAsUnchangeable();
            s.mode = .ViewWriteSome;
            return s;
        }

        pub fn getCell(self: *const Self, i: usize, j: usize) Cell {
            std.debug.assert(i < n or j < n);
            return self.data[i * n + j];
        }

        pub fn setCell(self: *Self, i: usize, j: usize, v: ?u8) !void {
            if (self.mode == .ViewOnly) {
                return;
            }
            std.debug.assert(i < n or j < n);
            if (!self.data[i * n + j].canChange and self.mode != .ViewWriteAll) {
                return;
            }
            const prev = self.data[i * n + j];
            self.data[i * n + j].setData(v);
            try self.trackAction(.{ .cell = .{ .prev = prev, .after = self.data[i * n + j] } });
        }

        // this action is not tracked in the undo-redo chain
        pub fn setCellUnlogged(self: *Self, i: usize, j: usize, v: ?u8) void {
            std.debug.assert(i < n or j < n);
            if (!self.data[i * n + j].canChange and self.mode != .ViewWriteAll) {
                return;
            }
            self.data[i * n + j].setData(v);
        }

        pub fn setCellAtPointer(self: *Self, v: ?u8) !void {
            try self.setCell(self.pointer.y, self.pointer.x, v);
        }

        pub fn setCellWhole(self: *Self, i: usize, j: usize, v: Cell) !void {
            std.debug.assert(i < n or j < n);
            if (!self.data[i * n + j].canChange) {
                return;
            }
            self.data[i * n + j] = v;
        }

        // Duty of the callee to flush if writer is a buffered writer
        pub fn printBoard(self: *const Self, writer: anytype, ws: ?TermSize) !void {
            if (ws != null) {
                try moveCursor(writer, ws.?.h / 2 - 8, 0);
            }
            for (0..n) |row| {
                if (row % self.nSqrt == 0 and row != 0) {
                    _ = try writer.write("\n");
                    const count = 2 * (n + 2) + 1; // just some formula that just works lol
                    if (ws != null) {
                        try moveCursorForward(writer, (ws.?.w - @as(u16, @intCast(count))) / 2);
                    }
                    _ = try writer.write("—" ** count);
                }

                for (0..n) |col| {
                    const i = row * n + col;

                    if (col % self.nSqrt == 0 and col != 0) {
                        _ = try writer.write(" |");
                    }
                    if (col % n == 0) {
                        _ = try writer.write("\n");

                        if (ws != null) {
                            try moveCursorForward(writer, ws.?.w / 2 - 10);
                        }
                    }
                    _ = try self.data[i].print(writer);
                }
            }
            _ = try writer.write("\n");
        }

        pub fn movePointerDown(self: *Self) !void {
            if (self.pointer.y == n - 1) {
                return;
            }
            try self.trackAction(.{ .move = self.pointer });
            self.pointer.y += 1;
        }
        pub fn movePointerUp(self: *Self) !void {
            if (self.pointer.y == 0) {
                return;
            }
            try self.trackAction(.{ .move = self.pointer });
            self.pointer.y -= 1;
        }

        pub fn movePointerRight(self: *Self) !void {
            if (self.pointer.x == n - 1) {
                return;
            }
            try self.trackAction(.{ .move = self.pointer });
            self.pointer.x += 1;
        }
        pub fn movePointerLeft(self: *Self) !void {
            if (self.pointer.x == 0) {
                return;
            }
            try self.trackAction(.{ .move = self.pointer });
            self.pointer.x -= 1;
        }

        pub fn update(self: *Self) void {
            const ptrPos = self.pointer.toBoardIndex(n);

            for (0..(n * n)) |i| {

                //var c = self.data[i];
                if (i == ptrPos) {
                    self.data[i].state.insert(CellState.Selected);
                } else {
                    self.data[i].state.remove(CellState.Selected);
                }

                if (self.data[i].value != null and !self.canBePlaced(i / 9, i % 9, self.data[i].value.?)) {
                    self.data[i].state.insert(CellState.Incorrect);
                } else {
                    self.data[i].state.remove(CellState.Incorrect);
                }
            }
        }

        pub fn getRow(self: *const Self, i: usize) [n]Cell {
            std.debug.assert(i < n);
            var res: [n]Cell = undefined;
            const start = i * n;
            for (start..start + n, 0..) |j, index| {
                res[index] = self.data[j];
            }
            return res;
        }

        pub fn getCol(self: *const Self, i: usize) [n]Cell {
            std.debug.assert(i < n);
            var res: [n]Cell = undefined;
            for (0..n) |j| {
                res[j] = self.data[j * n + i];
            }
            return res;
        }

        // get square with the given i,j value. i, j is the position of the square and NOT of the cell of which the square is a part of
        pub fn getSquare(self: *const Self, i: usize, j: usize) [n]Cell {
            std.debug.assert(i < n);
            std.debug.assert(j < n);
            const l_bound = self.nSqrt * j;
            const r_bound = l_bound + self.nSqrt;
            const u_bound = self.nSqrt * i;

            var out: [n]Cell = undefined;
            var count: usize = 0;
            for (0..3) |k| {
                const row = (k) + u_bound;
                for (l_bound..r_bound) |col| {
                    const c = self.getCell(row, col);
                    out[count] = c;
                    count += 1;
                }
            }
            return out;
        }

        // Group is the set of cells in a row, col or square of which the given cell is a part of
        pub fn getGroup(self: *const Self, i: usize, j: usize) [(n - std.math.sqrt(n)) * 3 + 2]Cell {
            std.debug.assert(i < n);
            std.debug.assert(j < n);
            var out: [(n - std.math.sqrt(n)) * 3 + 2]Cell = undefined;
            var index: usize = 0;
            const sq = self.getSquare(i / self.nSqrt, j / self.nSqrt);
            const row = self.getRow(i);
            const col = self.getCol(j);
            const cur = self.getCell(i, j);
            for (sq) |c| {
                if (std.meta.eql(cur, c)) {
                    continue;
                }
                out[index] = c;
                index += 1;
            }

            for (row) |c| {
                if (containsCell(&out, c, index) or std.meta.eql(cur, c)) {
                    continue;
                }
                out[index] = c;
                index += 1;
            }

            for (col) |c| {
                if (containsCell(&out, c, index) or std.meta.eql(cur, c)) {
                    continue;
                }
                out[index] = c;
                index += 1;
            }
            return out;
        }

        pub fn getGroupInt(self: *const Self, i: usize, j: usize) [(n - std.math.sqrt(n)) * 3 + 2]?u8 {
            var out = [_]?u8{null} ** ((n - std.math.sqrt(n)) * 3 + 2);
            const grp = self.getGroup(i, j);
            for (grp, 0..) |c, index| {
                out[index] = c.value;
            }
            return out;
        }
        // returns the number of unique bytes
        pub fn getGroupUniqueInt(self: *const Self, i: usize, j: usize, buf: []u8) usize {
            const grpInt = self.getGroupInt(i, j);
            var index: usize = 0;
            for (grpInt) |c| {
                if (c == null) {
                    continue;
                }
                if (std.mem.containsAtLeastScalar(u8, buf, 1, c.?)) {
                    continue;
                }
                buf[index] = c.?;
                index += 1;
            }
            return index;
        }

        pub fn isBoardValid(self: *const Self) bool {
            for (0..n) |row| {
                for (0..n) |col| {
                    const v = self.getCell(row, col);
                    if (v.value == null) {
                        continue;
                    }
                    if (!self.canBePlaced(row, col, v.value.?)) {
                        return false;
                    }
                }
            }
            return true;
        }

        pub fn canBePlaced(self: *const Self, i: usize, j: usize, v: u8) bool {
            const group = self.getGroup(i, j);
            for (group) |g| {
                if (g.value == null) {
                    continue;
                }
                if (g.value.? == v) {
                    return false;
                }
            }
            return true;
        }

        // solves the sudoku using brute force method (backtracking)
        pub fn solveBF(self: *Self) bool {
            for (0..n) |row| {
                for (0..n) |col| {
                    const c = self.getCell(row, col);
                    if (!c.canChange or c.value != null) {
                        continue;
                    }
                    for (1..10) |num| {
                        if (!self.canBePlaced(row, col, @intCast(num))) {
                            continue;
                        }
                        self.setCellUnlogged(row, col, @intCast(num));

                        if (self.solveBF()) {
                            return true;
                        }

                        self.setCellUnlogged(row, col, null);
                    }
                    return false;
                }
            }
            return true;
        }
        pub fn solve(self: *Self) bool {
            if (!self.isBoardValid()) {
                return false;
            }
            const o = self.solveBF();
            if (!o) {
                return false;
            }
            self.mode = .ViewOnly;
            return true;
        }

        // one way to do this is check the current board with the one used in solve() but that wont work if there are more than one solutions (given our current implementation of solveBF)
        pub fn isSolved(self: *const Self) bool {
            for (0..n) |row| {
                for (0..n) |col| {
                    const c = self.getCell(row, col);
                    if (c.value == null) {
                        return false;
                    }
                    if (!self.canBePlaced(row, col, c.value.?)) {
                        return false;
                    }
                }
            }
            return true;
        }

        pub fn reset(self: *Self) void {
            for (0..n) |row| {
                for (0..n) |col| {
                    if ((self.getCell(row, col)).canChange == false) {
                        continue;
                    }
                    self.setCellUnlogged(row, col, null);
                }
            }
        }

        pub fn trackAction(self: *Self, action: Action) !void {
            if (self.actionsPointer > self.actionsBuf.items.len) {
                unreachable;
            }
            if (self.actionsPointer < self.actionsBuf.items.len) {
                self.actionsBuf.items[self.actionsPointer] = action;
            } else if (self.actionsPointer == self.actionsBuf.items.len) {
                try self.actionsBuf.append(action);
            }
            self.actionsPointer += 1;
        }

        pub fn undo(self: *Self) void {
            if (self.actionsPointer == 0) {
                return;
            }
            const action = self.actionsBuf.items[self.actionsPointer - 1];
            self.actionsPointer -= 1;
            switch (action) {
                .move => |p| {
                    self.pointer = p;
                },
                .cell => |c| {
                    self.data[c.prev.position].value = c.prev.value;
                },
            }
        }

        pub fn redo(self: *Self) void {
            if (self.actionsPointer == self.actionsBuf.items.len) {
                return;
            }
            const action = self.actionsBuf.items[self.actionsPointer];
            self.actionsPointer += 1;
            switch (action) {
                .move => |p| {
                    self.pointer = p;
                },
                .cell => |c| {
                    self.data[c.prev.position].value = c.after.value;
                },
            }
        }

        pub fn setNonNullsAsUnchangeable(self: *Self) void {
            for (0..self.data.len) |i| {
                if (self.data[i].value == null) {
                    self.data[i].canChange = true;
                } else {
                    self.data[i].canChange = false;
                }
            }
        }
    };
}

pub fn containsCell(cells: []const Cell, v: Cell, max_len: usize) bool {
    //std.debug.print("v.pos to find: {}\n",.{v.position});
    for (cells, 0..) |c, i| {
        if (i == max_len) {
            return false;
        }
        //  if(std.meta.eql(c,v)) { return true; }
        if (c.position == v.position) {
            return true;
        }
        //std.debug.print("c.pos = {}\n",.{c.position});
    }
    return false;
}

pub fn cellsToU8(cells: []const Cell, dst: [*]?u8) void {
    for (0..cells.len) |i| {
        dst[i] = cells[i].value;
    }
}

pub fn contains(slice: []const ?u8, v: ?u8) bool {
    for (slice) |d| {
        if (v == d) {
            return true;
        }
    }
    return false;
}

const BoardClassic = Board(9);

// currently works only with posix systems
// these settings would be enough for our use-case
/// Returns the original terminal settings
pub fn setTerminalRawMode() !std.posix.termios {
    const handle = std.io.getStdOut().handle;
    const og_term = try std.posix.tcgetattr(handle);
    var term = og_term;
    term.lflag.ECHO = false;
    term.lflag.ICANON = false;
    try std.posix.tcsetattr(handle, .FLUSH, term);
    term.cc[@intFromEnum(std.posix.V.MIN)] = 1;
    term.cc[@intFromEnum(std.posix.V.TIME)] = 0;
    return og_term;
}

pub fn resetTerminal(og_term: std.posix.termios) !void {
    const handle = std.io.getStdOut().handle;
    try std.posix.tcsetattr(handle, .FLUSH, og_term);
}

pub fn setAlternativeMode(writer: anytype) !void {
    _ = try writer.write("\x1b[?1049h");
}

pub fn unsetAlternativeMode(writer: anytype) !void {
    _ = try writer.write("\x1b[?1049l");
}

// Again, they are enough for our use case
pub const Key = union(enum) {
    // alphanumeric ascii characters (0 to 9, 'a'..'z, 'A'..'Z')
    char: u8,

    arrowUp: void,
    arrowDown: void,
    arrowLeft: void,
    arrowRight: void,

    backspace: void,
    esc: void,
    enter: void,

    unsupported: void,
};

pub fn read_key(reader: anytype) !Key {
    var buf: [6]u8 = undefined; // 6 is enough, for this program
    const len = try reader.read(&buf);
    if (len == 0) {
        unreachable;
    }
    switch (buf[0]) {
        // special keys
        '\x1b' => {
            if (len == 1) {
                return .esc;
            }
            if (buf[1] == '[') {
                if (len < 3) {
                    return .unsupported;
                }
                switch (buf[2]) {
                    'A' => return .arrowUp,
                    'B' => return .arrowDown,
                    'C' => return .arrowRight,
                    'D' => return .arrowLeft,
                    else => return .unsupported,
                }
            }
        },

        '\x0A' => return .enter,
        '\x7f' => return .backspace,
        0x30...0x39, 0x41...0x5A, 0x61...0x7A => return Key{ .char = buf[0] },
        else => {},
    }
    return .unsupported;
}

pub const TermSize = struct { w: u16, h: u16 };

pub fn getTermSize(term: posix.fd_t) ?TermSize {
    var ws: posix.winsize = undefined;
    const ioctl = posix.system.ioctl;

    if (ioctl(term, posix.T.IOCGWINSZ, @intFromPtr(&ws)) == -1) {
        return null;
    }
    return .{ .w = ws.col, .h = ws.row };
}

pub const GameType = enum {
    Playing,
    Solver,
};

pub const GameState = union(enum) {
    selection_menu: void,
    on_board: GameType,
    unselected: void,
};

pub fn moveCursor(writer: anytype, row: u16, col: u16) !void {
    var buf: [15]u8 = [_]u8{0} ** 15;
    _ = try std.fmt.bufPrint(&buf, "\x1b[{};{}H", .{ row, col });
    _ = try writer.write(&buf);
}

pub fn moveCursorForward(writer: anytype, by: u16) !void {
    var buf = [_]u8{0} ** 11;
    _ = try std.fmt.bufPrint(&buf, "\x1b[{}C", .{by});
    _ = try writer.write(&buf);
}

pub const Game = struct {
    const Self = @This();

    board: BoardClassic,
    state: GameState = .unselected,

    pub fn newClassic() Self {
        return Self{
            .board = BoardClassic.new(),
        };
    }

    pub fn print(self: *const Self, writer: anytype, win_size: TermSize) !void {

        // center
        switch (self.state) {
            .unselected => {
                try moveCursor(writer, win_size.h / 2, win_size.w / 2);
                const title = "Sudoku!!\n";
                const sub = "(terminal version)\n";
                const choose_title = "choose your move\n";
                const opt1 = "Play (1)\n";
                const opt2 = "Create new (2)\n";

                _ = try writer.write(title);
                try moveCursorForward(writer, (win_size.w - @as(u16, @intCast(sub.len - 1))) / 2);
                _ = try writer.write(sub);
                try moveCursorForward(writer, (win_size.w - @as(u16, @intCast(choose_title.len - 1))) / 2);
                _ = try writer.write(choose_title);

                try moveCursorForward(writer, (win_size.w - @as(u16, @intCast(opt1.len - 1))) / 2);
                _ = try writer.write(opt1);
                try moveCursorForward(writer, (win_size.w - @as(u16, @intCast(opt2.len - 1))) / 2);
                _ = try writer.write(opt2);
                //try moveCursor(writer,win_size.h/2,win_size.w/2);

            },

            .on_board => {
                try self.board.printBoard(writer, win_size);
            },

            .selection_menu => {
                try moveCursor(writer, win_size.h / 2, win_size.w / 2);
                _ = try writer.write("todo lol\n");
            },
        }
    }

    pub fn update(self: *Self, reader: anytype, writer: anytype, reset_prompt_shown: *bool) !bool {
        const c = try read_key(reader);

        switch (c) {
            .char => |nc| {
                if (nc == 'q' or nc == 'Q') {
                    return false;
                }
            },
            else => {},
        }
        switch (self.state) {
            .unselected => {
                switch (c) {
                    .char => |nc| {
                        if (nc == '1') {
                            self.state = .{ .on_board = GameType.Playing };
                            self.board = try BoardClassic.newRandom();
                        } else if (nc == '2') {
                            self.state = .{ .on_board = GameType.Solver };
                        }
                    },
                    else => {},
                }
            },

            .on_board => {
                try switch (c) {
                    .char => |nc| {
                        if (reset_prompt_shown.*) {
                            reset_prompt_shown.* = false;
                        }

                        try switch (nc) {
                            '1'...'9' => self.board.setCellAtPointer(nc - 48),
                            else => {},
                        };
                        if (nc == 'r' or nc == 'R') {
                            _ = try writer.write("Reset everything? (y/n) \n");
                            reset_prompt_shown.* = true;
                        }
                        if (nc == 'y' or nc == 'Y') {
                            self.board.reset();
                            reset_prompt_shown.* = false;
                        }
                        if (nc == 'U' or nc == 'u') {
                            self.board.undo();
                        }

                        if (nc == 'P' or nc == 'p') {
                            self.board.redo();
                        }
                        if (nc == 'S' or nc == 's') {
                            self.board.setNonNullsAsUnchangeable();
                            const status = self.board.solve();

                            if (!status) {
                                _ = try writer.write("Board is impossible to solve\n");
                            } else {
                                self.board.mode = .ViewOnly;
                            }
                        }
                    },
                    .arrowUp => self.board.movePointerUp(),
                    .arrowDown => self.board.movePointerDown(),
                    .arrowLeft => self.board.movePointerLeft(),
                    .arrowRight => self.board.movePointerRight(),
                    .backspace => self.board.setCellAtPointer(null),
                    .enter => {
                        const res = self.board.isSolved();
                        var buf: [17]u8 = undefined;
                        _ = try std.fmt.bufPrint(&buf, "Is Solved: {any}\n", .{res});
                        _ = try writer.write(&buf);
                    },
                    else => {},
                };
                self.board.update();
            },

            .selection_menu => {},
        }

        return true;
    }
};

pub fn main() !void {
    const og_term = try setTerminalRawMode();
    defer resetTerminal(og_term) catch {};

    const stdOut = std.io.getStdOut();
    var bufWriter = std.io.bufferedWriter(stdOut.writer());
    try setAlternativeMode(&bufWriter);
    try bufWriter.flush();

    //var b = BoardClassic.newFromNumSlice([_]?u8{ 5, 3, null, null, 7, null, null, null, null, 6, null, null, 1, 9, 5, null, null, null, null, 9, 8, null, null, null, null, 6, null, 8, null, null, null, 6, null, null, null, 3, 4, null, null, 8, null, 3, null, null, 1, 7, null, null, null, 2, null, null, null, 6, null, 6, null, null, null, null, 2, 8, null, null, null, null, 4, 1, 9, null, null, 5, null, null, null, null, 8, null, null, 7, 9 });
    //var b = BoardClassic.new();
    //try b.printBoard(&bufWriter);
    //try b.setCell(4,4,2);
    //b.update();
    // try b.printBoard(&bufWriter);
    //try bufWriter.flush();

    defer {
        unsetAlternativeMode(&bufWriter) catch {};
        bufWriter.flush() catch {};
    }

    //try b.solve();
    const tty = (try std.fs.cwd().openFile("/dev/tty", .{}));
    const reader = tty.reader();

    var reset_prompt_shown = false;
    var game = Game.newClassic();

    var ws = getTermSize(tty.handle);
    if (ws == null) {
        return;
    }
    try game.print(&bufWriter, ws.?);
    try bufWriter.flush();

    while (true) {
        ws = getTermSize(tty.handle);
        if (ws == null) {
            break;
        } // handle this, maybe lol im lazy
        //clear the screen
        _ = try bufWriter.write("\x1b[2J");

        if (!try game.update(&reader, &bufWriter, &reset_prompt_shown)) {
            break;
        }
        try game.print(&bufWriter, ws.?);
        try bufWriter.flush();
    }

    return;
}

test "getCell" {
    const b = BoardClassic.newFromNumSlice([_]?u8{ 5, 3, null, null, 7, null, null, null, null, 6, null, null, 1, 9, 5, null, null, null, null, 9, 8, null, null, null, null, 6, null, 8, null, null, null, 6, null, null, null, 3, 4, null, null, 8, null, 3, null, null, 1, 7, null, null, null, 2, null, null, null, 6, null, 6, null, null, null, null, 2, 8, null, null, null, null, 4, 1, 9, null, null, 5, null, null, null, null, 8, null, null, 7, 9 });
    const cell = b.getCell(5, 4);

    const exp = 2;
    try std.testing.expect(cell.value == exp);
}

test "getRow" {
    const b = BoardClassic.newFromNumSlice([_]?u8{ 5, 3, null, null, 7, null, null, null, null, 6, null, null, 1, 9, 5, null, null, null, null, 9, 8, null, null, null, null, 6, null, 8, null, null, null, 6, null, null, null, 3, 4, null, null, 8, null, 3, null, null, 1, 7, null, null, null, 2, null, null, null, 6, null, 6, null, null, null, null, 2, 8, null, null, null, null, 4, 1, 9, null, null, 5, null, null, null, null, 8, null, null, 7, 9 });
    const row0 = b.getRow(8);
    var row0_pure: [9]?u8 = undefined;
    for (0..9) |i| {
        row0_pure[i] = row0[i].value;
    }
    const exp_row = [_]?u8{ null, null, null, null, 8, null, null, 7, 9 };
    try std.testing.expect(std.mem.eql(?u8, &row0_pure, &exp_row));
}

test "getCol" {
    const b = BoardClassic.newFromNumSlice([_]?u8{ 5, 3, null, null, 7, null, null, null, null, 6, null, null, 1, 9, 5, null, null, null, null, 9, 8, null, null, null, null, 6, null, 8, null, null, null, 6, null, null, null, 3, 4, null, null, 8, null, 3, null, null, 1, 7, null, null, null, 2, null, null, null, 6, null, 6, null, null, null, null, 2, 8, null, null, null, null, 4, 1, 9, null, null, 5, null, null, null, null, 8, null, null, 7, 9 });
    const col0 = b.getCol(0);
    var col0_pure: [9]?u8 = undefined;
    for (0..9) |i| {
        col0_pure[i] = col0[i].value;
    }
    const exp_col = [_]?u8{ 5, 6, null, 8, 4, 7, null, null, null };
    try std.testing.expect(std.mem.eql(?u8, &col0_pure, &exp_col));
}

test "getSquare" {
    const b = BoardClassic.newFromNumSlice([_]?u8{ 5, 3, null, null, 7, null, null, null, null, 6, null, null, 1, 9, 5, null, null, null, null, 9, 8, null, null, null, null, 6, null, 8, null, null, null, 6, null, null, null, 3, 4, null, null, 8, null, 3, null, null, 1, 7, null, null, null, 2, null, null, null, 6, null, 6, null, null, null, null, 2, 8, null, null, null, null, 4, 1, 9, null, null, 5, null, null, null, null, 8, null, null, 7, 9 });
    const square0 = b.getSquare(2, 0);
    var square0_pure: [9]?u8 = undefined;
    for (0..9) |i| {
        square0_pure[i] = square0[i].value;
    }
    const exp_square = [_]?u8{ null, 6, null, null, null, null, null, null, null };
    try std.testing.expect(std.mem.eql(?u8, &square0_pure, &exp_square));
}
test "getGroupSet" {
    const b = BoardClassic.newFromNumSlice([_]?u8{ 5, 3, null, null, 7, null, null, null, null, 6, null, null, 1, 9, 5, null, null, null, null, 9, 8, null, null, null, null, 6, null, 8, null, null, null, 6, null, null, null, 3, 4, null, null, 8, null, 3, null, null, 1, 7, null, null, null, 2, null, null, null, 6, null, 6, null, null, null, null, 2, 8, null, null, null, null, 4, 1, 9, null, null, 5, null, null, null, null, 8, null, null, 7, 9 });
    const square0 = b.getGroup(6, 0);
    var square0_pure: [20]?u8 = undefined;
    for (0..20) |i| {
        square0_pure[i] = square0[i].value;
    }
    const exp_square = [_]?u8{ 6, null, null, null, null, null, null, null, null, null, null, 2, 8, null, 5, 6, null, 8, 4, 7 };
    //std.debug.print("Expected: {any} ; got: {any}\n",.{exp_square,square0_pure});
    try std.testing.expect(std.mem.eql(?u8, &square0_pure, &exp_square));
}

test "canBePlaced" {
    const b = BoardClassic.newFromNumSlice([_]?u8{ 5, 3, null, null, 7, null, null, null, null, 6, null, null, 1, 9, 5, null, null, null, null, 9, 8, null, null, null, null, 6, null, 8, null, null, null, 6, null, null, null, 3, 4, null, null, 8, null, 3, null, null, 1, 7, null, null, null, 2, null, null, null, 6, null, 6, null, null, null, null, 2, 8, null, null, null, null, 4, 1, 9, null, null, 5, null, null, null, null, 8, null, null, 7, 9 });
    const ans = b.canBePlaced(8, 6, 6);
    const exp_ans = true;
    try std.testing.expect(ans == exp_ans);
}

test "isSolved" {
    const b = BoardClassic.newFromNumSlice([_]?u8{ 5, 3, 4, 6, 7, 8, 9, 1, 2, 6, 7, 2, 1, 9, 5, 3, 4, 8, 1, 9, 8, 3, 4, 2, 5, 6, 7, 8, 5, 9, 7, 6, 1, 4, 2, 3, 4, 2, 6, 8, 5, 3, 7, 9, 1, 7, 1, 3, 9, 2, 4, 8, 5, 6, 9, 6, 1, 5, 3, 7, 2, 8, 4, 2, 8, 7, 4, 1, 9, 6, 3, 5, 3, 4, 5, 2, 8, 6, 1, 7, 9 });
    const ans = b.isSolved();
    try std.testing.expect(ans == true);
}

test "isSolved Wrong" {
    const b = BoardClassic.newFromNumSlice([_]?u8{ 1, 2, 3, 2, 1, 3, 2, 1, 3, 4, 5, 6, 4, 5, 6, 4, 5, 6, 7, 8, 9, 7, 8, 9, 7, 8, 9, 2, 1, 3, 1, 2, 3, 1, 2, 3, 4, 5, 6, 4, 5, 6, 4, 5, 6, 7, 8, 9, 7, 8, 9, 7, 8, 9, 2, 1, 3, 1, 2, 3, 1, 2, 3, 4, 5, 6, 4, 5, 6, 4, 5, 6, 7, 8, 9, 7, 8, 9, 7, 8, 9 });
    const ans = b.isSolved();
    try std.testing.expect(ans == false);
}
