const std = @import("std");
const ally = std.heap.page_allocator; // zero memory handling, need to add free memory code
const EnvEntry = struct { name: []const u8, value: Lisp.LispValue };
const Env = std.SinglyLinkedList(EnvEntry);

const Lisp = struct {
    env: Env, // functions, symbols
    rng: std.Random.DefaultPrng,

    const Builtins = [_]EnvEntry{
        .{ .name = "true", .value = .{.boolean = true}},
        .{ .name = "#t", .value = .{.boolean = true}},
        .{ .name = "false", .value = .{.boolean = false}},
        .{ .name = "#f", .value = .{.boolean = false}},
        .{ .name = "nil", .value = .{.boolean = false}},
        .{ .name = "print", .value = .{.builtin = lisp_print}},
        .{ .name = "eq", .value = .{.builtin = lisp_eq}},
        .{ .name = "car", .value = .{.builtin = lisp_car}},
        .{ .name = "cdr", .value = .{.builtin = lisp_cdr}},
        .{ .name = "atom", .value = .{.builtin = lisp_atom}},
        .{ .name = "cons", .value = .{.builtin = lisp_cons}},
        .{ .name = "progn", .value = .{.builtin = lisp_progn}},
        .{ .name = "format", .value = .{.builtin = lisp_format}},
        .{ .name = "rand", .value = .{.builtin = lisp_rand}},
        .{ .name = "macroexpand", .value = .{.builtin = lisp_macroexpand}},
        .{ .name = "macroexpand-1", .value = .{.builtin = lisp_macroexpand_1}},
        .{ .name = "+", .value = .{.builtin = lisp_plus}},
        .{ .name = "-", .value = .{.builtin = lisp_minus}},
        .{ .name = "*", .value = .{.builtin = lisp_mul}},
        .{ .name = "/", .value = .{.builtin = lisp_div}},
    };

    fn lisp_print(_: *Lisp, args: []const LispValue) !LispValue {
        std.debug.print("{s}\n", .{args[0].string}); 
        return LispValue{.symbol = "nil"};
    }

    fn lisp_eq(self: *Lisp, args: []const LispValue) !LispValue {
        const ltrue = LispValue{.boolean = true};
        const lfalse = LispValue{.boolean = false};
        if (std.meta.activeTag(args[0]) != std.meta.activeTag(args[1])) return lfalse;
        switch (args[0]) {
            .boolean => return if (args[0].boolean == args[1].boolean) ltrue else lfalse,
            .number => return if (args[0].number == args[1].number) ltrue else lfalse,
            .string => return if (std.mem.eql(u8, args[0].string, args[1].string)) ltrue else lfalse,
            .symbol => return if (std.mem.eql(u8, args[0].symbol, args[1].symbol)) ltrue else lfalse,
            .list => {
                if (args[0].list.len != args[1].list.len) return lfalse;
                for (args[0].list, args[1].list) |a0, a1| {
                    if ((try lisp_eq(self, &[_]LispValue{a0, a1})).boolean == false) return lfalse;
                }
                return ltrue;
            },
            .function => return self.lisp_eq(&[_]LispValue{.{.list = args[0].function.value}, .{.list = args[1].function.value}}),
            .macro => return self.lisp_eq(&[_]LispValue{.{.list = args[0].macro.value}, .{.list = args[1].macro.value}}),
            .builtin => return if (args[0].builtin == args[1].builtin) ltrue else lfalse,
        }
    }

    fn lisp_car(_: *Lisp, args: []const LispValue) !LispValue {
        if (args[0] != .list) return error.CarNeedList;
        return args[0].list[0];
    }

    fn lisp_cdr(_: *Lisp, args: []const LispValue) !LispValue {
        if (args[0] != .list) return error.CdrNeedList;
        return LispValue{.list = args[0].list[1..]};
    }

    fn lisp_atom(_: *Lisp, args: []const LispValue) !LispValue {
        return switch (args[0]) {
            .symbol, .number, .string, .boolean, .function, .macro, .builtin => LispValue{.boolean = true},
            .list => LispValue{.boolean = false},
        };
    }

    fn lisp_cons(_: *Lisp, args: []const LispValue) !LispValue {
        if (args.len != 2) return error.ConsExpectTwoArguments;
        var list = std.ArrayList(LispValue).init(ally);
        const first = args[0];
        const last = args[1];
        try list.append(first);
        if (last == .boolean and last.boolean == false) {
            return LispValue{.list = try list.toOwnedSlice()};
        }
        if (last == .list) {
            try list.appendSlice(last.list);
        } else {
            try list.append(last);
        }
        return LispValue{.list = try list.toOwnedSlice()};
    }

    fn lisp_progn(self: *Lisp, args: []const LispValue) !LispValue {
        var rc = LispValue{.symbol = "nil"};
        for (args) |arg| {
            rc = try self.eval(arg);
        }
        return rc;
    }

    fn lisp_format(_: *Lisp, args: []const LispValue) !LispValue {
        const output = args[0].boolean; // if true write to stdout else return as str
        const fmt = args[1].string;
        var vars = args[2..];
        var buff = std.ArrayList(u8).init(ally);

        var index: usize = 0;
        while (index < fmt.len) {
            switch (fmt[index]) {
                '~' => {
                    index += 1; // skip ~
                    const specifier = fmt[index];
                    var writer = buff.writer();
                    switch (specifier) {
                        '~' => try buff.append('~'),
                        '%' => try buff.append('\n'),
                        'A' => { // pretty print
                            try writer.print("{s}", .{vars[0]});
                            vars = vars[1..];
                        },
                        'R' => { // "raw" format
                            try writer.print("{any}", .{vars[0]});
                            vars = vars[1..];
                        },
                        else => return error.UnknownFormatSpecifier,
                    }
                    index += 1;
                },
                else => |c| {
                    try buff.append(c);
                    index += 1;
                },
            }
        }

        const str = try buff.toOwnedSlice();
        if (!output) return LispValue{.string = str};
        std.debug.print("{s}", .{str});
        return LispValue{.symbol = "nil"};
    }

    fn lisp_rand(self: *Lisp, args: []const LispValue) !LispValue {
        const rng = self.rng.random();
        return switch (args.len) {
            0 => .{.number = rng.float(f64)},
            1 => .{.number = @floatFromInt(rng.intRangeAtMost(i64, 0, @intFromFloat(args[0].number)))},
            else => .{.number = @floatFromInt(rng.intRangeAtMost(i64, @intFromFloat(args[0].number), @intFromFloat(args[1].number)))},
        };
    }

    fn lisp_macroexpand(self: *Lisp, args: []const LispValue) !LispValue {
        var previous = args[0];
        while (true) {
            const next = try self.lisp_macroexpand_1(&[_]LispValue{previous});
            if ((try self.lisp_eq(&[_]LispValue{previous, next})).boolean) break;
            previous = next;
        }
        return previous;
    }

    fn lisp_macroexpand_1(self: *Lisp, args: []const LispValue) !LispValue {
        if (args[0] != .list) return args[0];
        switch (args[0].list[0]) {
            .symbol => |symbol| {
                if (self.envGet(symbol)) |sym| {
                    if (sym != .macro) { // check inside the list for macro
                        var newlist = std.ArrayList(LispValue).init(ally);
                        for (0..args[0].list.len) |i| { // for each element of list check if macro
                            const next = try self.lisp_macroexpand_1(&[_]LispValue{args[0].list[i]});
                            // if yes (because changed) add it to the new list and append next unchanged
                            if (!(try self.lisp_eq(&[_]LispValue{args[0].list[i], next})).boolean) {
                                try newlist.append(next);
                                try newlist.appendSlice(args[0].list[i+1..]);
                                break;
                            }
                            try newlist.append(args[0].list[i]);
                        }
                        return LispValue{.list = try newlist.toOwnedSlice()};
                    }
                    // args[0] is a macro call
                    if (sym.macro.args.len != args[0].list[1..].len) return error.MacroWrongNumberOfArguments;
                    const backup_env = self.env; // to reset env later
                    for (sym.macro.args, args[0].list[1..]) |name, val| try self.envAdd(name, val);
                    var rc: LispValue = .{ .symbol =  "nil"};
                    for (sym.macro.value) |val| rc = try self.eval(val); // only eval once
                    self.env = backup_env; // reset env
                    return rc;
                }
                return args[0];
            },
            else => return args[0],
        }
    }

    fn lisp_plus(_: *Lisp, args: []const LispValue) !LispValue {
        return LispValue{.number = args[0].number + args[1].number};
    }
    fn lisp_minus(_: *Lisp, args: []const LispValue) !LispValue {
        return LispValue{.number = args[0].number - args[1].number};
    }
    fn lisp_mul(_: *Lisp, args: []const LispValue) !LispValue {
        return LispValue{.number = args[0].number * args[1].number};
    }
    fn lisp_div(_: *Lisp, args: []const LispValue) !LispValue {
        return LispValue{.number = args[0].number / args[1].number};
    }

    pub fn lisp_define(self: *Lisp, args: []const LispValue) anyerror!void {
        if (args[0] == .symbol) {
            if (args[1] != .list) {
                try self.envAdd(args[0].symbol, args[1]);
            } else { // (define x (lambda () true)) ; for example
                try self.envAdd(args[0].symbol, try self.eval(args[1]));
            }
        } else if(args[0] == .list) { // (define (x) (print x))
            const fname = args[0].list[0].symbol;
            const fargs = args[0].list[1..];
            var strargs = std.ArrayList([]const u8).init(ally);
            for (fargs) |farg| try strargs.append(farg.symbol);
            const body = args[1..];
            try self.envAdd(fname, .{ .function = .{ .args = try strargs.toOwnedSlice(), .env = self.env, .value = body } });
            self.env.first.?.data.value.function.env = self.env; // add itself to env for recursive calls
        }
    }

    pub fn lisp_defmacro(self: *Lisp, args: []const LispValue) !void {
        if (args[0] != .list) return error.DefineMacroExpectList;
        const fname = args[0].list[0].symbol;
        const fargs = args[0].list[1..];
        var strargs = std.ArrayList([]const u8).init(ally);
        for (fargs) |farg| try strargs.append(farg.symbol);
        const body = args[1..];
        try self.envAdd(fname, .{ .macro = .{ .args = try strargs.toOwnedSlice(), .value = body } });
    }

    pub fn lisp_lambda(self: *Lisp, args: []const LispValue) !LispValue {
        if(args[0] == .list) {
            const fargs = args[0].list;
            var strargs = std.ArrayList([]const u8).init(ally);
            for (fargs) |farg| try strargs.append(farg.symbol);
            const body = args[1..];
            return LispValue{.function = .{ .args = try strargs.toOwnedSlice(), .env = self.env, .value = body }};
        }
        return error.InvalidLambda;
    }

    pub fn lisp_quasiquote(self: *Lisp, args: LispValue) !LispValue {
        if (args != .list) return args;
        var newlist = std.ArrayList(LispValue).init(ally);
        for (args.list) |value| {
            switch (value) {
                .list => |list| {
                    if (list[0] == .symbol and std.mem.eql(u8, list[0].symbol, "unquote")) {
                        try newlist.append(try self.eval(list[1]));
                    } else if (list[0] == .symbol and std.mem.eql(u8, list[0].symbol, "unquote-splat")) {
                        switch(list[1]) {
                            .list => |l| try newlist.appendSlice(l),
                            .symbol => try newlist.appendSlice((try self.eval(list[1])).list),
                            else => return error.SplatExpectListOrSymbol,
                        }
                    } else {
                        try newlist.append(try self.lisp_quasiquote(value));
                    }
                },
                else => |val| try newlist.append(val),
            }
        }
        return LispValue{.list = try newlist.toOwnedSlice()};
    }

    pub fn init() !Lisp {
        var lisp = Lisp{ .env = .{}, .rng = .init(@intCast(std.time.nanoTimestamp())) };
        var next: ?*Env.Node = null;
        for (Lisp.Builtins) |builtin| {
            const node = try ally.create(Env.Node);
            node.* = .{ .data = builtin, .next = next };
            next = node;
            lisp.env.prepend(node);
        }
        return lisp;
    }

    // resolve string to Symbol from Env
    fn envGet(self: *Lisp, symbol: []const u8) ?LispValue {
        var p = self.env.first;
        while (p) |n| : (p = p.?.next) {
            if (std.mem.eql(u8, n.data.name, symbol)) return n.data.value;
        }
        return null;
    }

    // return pointer to node of env element
    // (for example used for set!)
    fn envFind(self: *Lisp, symbol: []const u8) ?*Env.Node {
        var p = self.env.first;
        while (p) |n| : (p = p.?.next) {
            if (std.mem.eql(u8, n.data.name, symbol)) return n;
        }
        return null;
    }

    // add var/func to env
    fn envAdd(self: *Lisp, name: []const u8, value: LispValue) !void {
        const node = try ally.create(Env.Node);
        node.* = .{.data = EnvEntry{.name = name, .value = value}, .next = self.env.first};
        self.env.prepend(node);
    }

    // call builtin/function
    pub fn call(self: *Lisp, list: []const LispValue) anyerror!LispValue {
        const value = self.envGet(list[0].symbol) orelse {
            std.debug.print("{}: undefined function\n", .{list[0]});
            return error.UndefinedFunction;
        };
        var fnargs = list[1..]; // workaround for tail call
        func: switch (value) {
            .function => |function| {
                var args = std.ArrayList(LispValue).init(ally);
                for (fnargs) |arg| try args.append(try self.eval(arg));
                if (function.args.len != args.items.len) return error.FunctionWrongNumberOfArguments;
                const backup_env = self.env;
                self.env = function.env;
                // before reset env need to free each node until we reach backup_env
                for (function.args, args.items) |name, val| try self.envAdd(name, val);
                var rc: LispValue = .{ .symbol =  "nil"};
                // need to free each rc depending on if its a str/list/...
                // except last one function.value[0..function.value-1]
                // return the last rc
                for (function.value[0..function.value.len-1]) |val| rc = try self.eval(val);
                const last = function.value[function.value.len-1];
                if (last == .list and last.list[0] == .symbol and std.mem.eql(u8, last.list[0].symbol, list[0].symbol)) {
                    // change args
                    fnargs = last.list[1..];
                    continue :func value;
                } else {
                    rc = try self.eval(last);
                }
                self.env = backup_env;
                return rc;
            },
            .macro => |macro| {
                if (macro.args.len != list[1..].len) return error.MacroWrongNumberOfArguments;
                // before reset env need to free each node until we reach backup_env
                const backup_env = self.env; // to reset env later
                for (macro.args, list[1..]) |name, val| try self.envAdd(name, val);
                var rc: LispValue = .{ .symbol =  "nil"};
                // need to free each rc depending on if its a str/list/...
                // except last one macro.value[0..macro.value-1]
                // return the last rc
                for (macro.value) |val| rc = try self.eval(try self.eval(val));
                self.env = backup_env; // reset env
                return rc;
            },
            .builtin => |builtin| {
                // need to free the args list
                // and each LispValue inside depending on if its a str/list/...
                var args = std.ArrayList(LispValue).init(ally);
                defer args.deinit();
                for (list[1..]) |arg| try args.append(try self.eval(arg));
                const rc = builtin(self, args.items);
                return rc;
            },
            .list => return try self.eval(value),
            else => |e| {std.debug.print("got: {any}\n", .{e}); return error.ExpectedFunction; },
        }
    }

    // eval a LispValue
    pub fn eval(self: *Lisp, value: LispValue) !LispValue {
        ev: switch (value) {
            .number, .string, .boolean => return value, // self evaluate
            .symbol => |symbol| return self.envGet(symbol) orelse {
                std.debug.print("got: {s}\n", .{symbol}); return error.UnboundVariable; },
            .list => |list| { // handle list, function call, special forms
                if (list.len == 0) return LispValue{.symbol = "nil"};

                switch(list[0]) {
                    .symbol => |symbol| {
                        if (std.mem.eql(u8, symbol, "define")) {
                            try self.lisp_define(list[1..]);
                        } else if (std.mem.eql(u8, symbol, "defmacro")) {
                            try self.lisp_defmacro(list[1..]);
                        } else if (std.mem.eql(u8, symbol, "lambda")) {
                            return try self.lisp_lambda(list[1..]);
                        } else if (std.mem.eql(u8, symbol, "if")) {
                            const boolean = try self.eval(list[1]);
                            if (boolean == .boolean and boolean.boolean == true) {
                                continue :ev list[2];
                            } else if (list.len > 2) {
                                continue :ev list[3];
                            }
                        } else if (std.mem.eql(u8, symbol, "quote")) {
                            return list[1];
                        } else if (std.mem.eql(u8, symbol, "quasiquote")) {
                            return self.lisp_quasiquote(list[1]);
                        } else if (std.mem.eql(u8, symbol, "set!")) {
                            const target = self.envFind(list[1].symbol) orelse return error.UnboundVariable;
                            target.data.value = try self.eval(list[2]);
                            return target.data.value;
                        } else if (std.mem.eql(u8, symbol, "cond")) {
                            for (list[1..]) |combo| {
                                if (combo != .list) return error.CondExpectLists;
                                const booleanfn = combo.list[0];
                                const retvalue = combo.list[1];
                                if ((try self.eval(booleanfn)).boolean == true) {
                                    continue :ev retvalue;
                                }
                            }
                        } else {
                            return try self.call(list);
                        }
                        return LispValue{.symbol = "nil"};
                    },
                    else => |e| { std.debug.print("got: {any}\n", .{e}); return error.InvalidFunction; },
                }
            },
            else => return error.InvalidFunction, // couldnt have a function here yet
        }
    }

    // parse list of tokens to a LispValue
    pub const Quote = enum { None, Quote, QuasiQuote };
    pub fn parse(self: *Lisp, tokens: *std.ArrayList(Token), index: *usize, quote: Quote) !LispValue {
        if (index.* == tokens.items.len) return error.EOF;
        switch (tokens.items[index.*]) {
            .lpar => {
                index.* += 1;
                var list = std.ArrayList(LispValue).init(ally);
                while (index.* < tokens.items.len and tokens.items[index.*] != .rpar) {
                    const value = try self.parse(tokens, index, quote);
                    try list.append(value);
                }
                if (index.* == tokens.items.len) return error.UnclosedParen;
                index.* += 1; // consume ')'
                return LispValue{.list = try list.toOwnedSlice()};
            },
            .rpar => return error.UnexpectedRPar,
            .LispValue => |lvalue| {
                if (lvalue == .symbol and quote == .None and std.mem.eql(u8, lvalue.symbol, "'")) {
                    index.* += 1;
                    const value = try self.parse(tokens, index, .Quote);
                    var list = std.ArrayList(LispValue).init(ally);
                    try list.append(LispValue{.symbol = "quote"});
                    try list.append(value);
                    return LispValue{ .list = try list.toOwnedSlice() };
                }
                if (lvalue == .symbol and quote == .None and std.mem.eql(u8, lvalue.symbol, "`")) {
                    index.* += 1;
                    const value = try self.parse(tokens, index, .QuasiQuote);
                    var list = std.ArrayList(LispValue).init(ally);
                    try list.append(LispValue{.symbol = "quasiquote"});
                    try list.append(value);
                    return LispValue{ .list = try list.toOwnedSlice() };
                }
                if (lvalue == .symbol and quote == .QuasiQuote and std.mem.eql(u8, lvalue.symbol, ",")) {
                    index.* += 1;
                    if (tokens.items[index.*] == .LispValue and tokens.items[index.*].LispValue == .symbol and std.mem.eql(u8, tokens.items[index.*].LispValue.symbol, "@")) {
                        index.* += 1;
                        const value = try self.parse(tokens, index, .Quote);
                        var list = std.ArrayList(LispValue).init(ally);
                        try list.append(LispValue{.symbol = "unquote-splat"});
                        try list.append(value);
                        return LispValue{ .list = try list.toOwnedSlice() };
                    } else {
                        const value = try self.parse(tokens, index, .None);
                        var list = std.ArrayList(LispValue).init(ally);
                        try list.append(LispValue{.symbol = "unquote"});
                        try list.append(value);
                        return LispValue{ .list = try list.toOwnedSlice() };
                    }
                }
                index.* += 1; return lvalue;
            },
        }
        return error.InvalidToken;
    }

    const LispValue = union(enum) {
        number: f64,
        string: []const u8,
        boolean: bool,
        symbol: []const u8,
        list: []const LispValue,
        function: struct {
            args: []const []const u8,
            value: []const LispValue,
            env: Env,
        },
        macro: struct {
            args: []const []const u8,
            value: []const LispValue,
        },
        builtin: *const fn(L: *Lisp, args: []const LispValue) anyerror!LispValue,

        pub fn format(self: LispValue, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            const pretty = fmt.len > 0 and fmt[0] == 's';
            if (pretty) {
                switch(self) {
                    .string, .symbol => |sym| try writer.print("{s}", .{sym}),
                    .number => |n| try writer.print("{d:.2}", .{n}),
                    .boolean => |b| try writer.print("{}", .{b}),
                    .list => |l| {
                        var sep: []const u8 = " ";
                        try writer.print("[", .{});
                        for (l) |v| {
                            try writer.print("{s}{s}", .{sep, v});
                            sep = ", ";
                        }
                        try writer.print(" ]", .{});
                    },
                    .function => |f| try writer.print("Fn({s}){{ {s} }}", .{f.args, f.value}),
                    .macro => |m| try writer.print("Macro({s}){{ {s} }}", .{m.args, m.value}),
                    else => try writer.print("{}", .{self}),
                }
            } else {
                switch(self) {
                    .number => |n| try writer.print("Number({d:.2})", .{n}),
                    .string => |str| try writer.print("String(\"{s}\")", .{str}),
                    .boolean =>|b| try writer.print("Boolean({})", .{b}),
                    .symbol => |i| try writer.print("Symbol(\"{s}\")", .{i}),
                    .list => |l|  {
                        if (pretty) {
                            try writer.print("List({s})", .{l});
                        } else {
                            try writer.print("List({any})", .{l});
                        }
                    },
                    .function => |f| try writer.print("Fn({s}){{ {any} }}", .{f.args, f.value}),
                    .macro => |m| try writer.print("Macro({s}){{ {any} }}", .{m.args, m.value}),
                    .builtin => try writer.print("Builtin", .{})
                }
            }
        }
    };

    const Token = union(enum) {
        lpar: void,
        rpar: void,
        LispValue: LispValue,

        pub const Iterator = struct {
            data: []const u8,

            pub fn init(data: []const u8) Iterator {
                return .{ .data = data };
            }

            fn next(self: *Iterator) !?Token {
                while (self.data.len > 0) switch (self.data[0]) {
                    ' ', '\t', '\r', '\n' => {
                        self.data = self.data[1..];
                        continue;
                    },
                    '(', '[' => {
                        self.data = self.data[1..];
                        return .lpar;
                    },
                    ')', ']' => {
                        self.data = self.data[1..];
                        return .rpar;
                    },
                    ';' => { // comment
                        const end = std.mem.indexOfAny(u8, self.data, "\n") orelse self.data.len;
                        self.data = self.data[end..];
                        continue;
                    },
                    '"' => { // string
                        const end = std.mem.indexOfScalar(u8, self.data[1..], '"') orelse return error.MissingStringQuoteEnd;
                        const str = self.data[1 .. end + 1]; // skip quote start
                        self.data = self.data[end + 2 ..]; // skip quote end
                        return .{ .LispValue = .{ .string = try ally.dupe(u8, str) } };
                    },
                    '\'' => {
                        self.data = self.data[1..];
                        return .{ .LispValue = .{ .symbol = "'" } };
                    },
                    ',' => {
                        self.data = self.data[1..];
                        return .{ .LispValue = .{ .symbol = "," } };
                    },
                    '@' => {
                        self.data = self.data[1..];
                        return .{ .LispValue = .{ .symbol = "@" } };
                    },
                    else => { // need to check if bool, num or identifier
                        const end = std.mem.indexOfAny(u8, self.data, " ()\n") orelse self.data.len;
                        const data = self.data[0..end];
                        self.data = self.data[end..];

                        if (std.mem.eql(u8, data, "true")) {
                            return .{ .LispValue = .{ .boolean = true } };
                        } else if (std.mem.eql(u8, data, "false")) {
                            return .{ .LispValue = .{ .boolean = false } };
                        }
                        const num = std.fmt.parseFloat(f64, data) catch {
                            return .{ .LispValue = .{ .symbol = try ally.dupe(u8, data) } };
                        };
                        return .{ .LispValue = .{ .number = num } };
                    },
                };
                return null;
            }
        };
    };
};

// tokenize/parse/eval in a loop until EOF
pub fn repl(lisp: *Lisp, data: []const u8) !Lisp.LispValue {
    var it = Lisp.Token.Iterator.init(data);
    var tokens = std.ArrayList(Lisp.Token).init(ally);
    var index: usize = 0;
    var rc: Lisp.LispValue = undefined;
    while (try it.next()) |token| try tokens.append(token);
    while (true) {
        const val = lisp.parse(&tokens, &index, .None) catch |err| {
            if (err == error.EOF) break;
            return err;
        };
        rc = try lisp.eval(val);
    }
    return rc;
}

test "basic" {
    var lisp = try Lisp.init();
    const rc = repl(&lisp, "(+ 2 (+ 1 1))");
    try std.testing.expectEqual(rc, Lisp.LispValue{.number = 4});
}

pub fn main() !void {
    var lisp = try Lisp.init();
    if (std.os.argv.len == 2) {
        const data = try std.fs.cwd().readFileAlloc(ally, std.mem.span(std.os.argv[1]), 10 * 1024 * 1024);
        defer ally.free(data);
        const rc = try repl(&lisp, data);
        _ = rc;
    } else {
        var stdin = std.io.getStdIn().reader();
        var buff: [1024]u8 = undefined;
        std.debug.print("> ", .{});
        while (try stdin.readUntilDelimiterOrEof(&buff, '\n')) |line| {
            const rc = try repl(&lisp, line);
            std.debug.print(";; {}\n> ", .{rc});
        }
        std.debug.print("\rBye !\n", .{});
    }
}
