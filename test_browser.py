"""Compile first, then run with Python Playwright; GARDEN_BROWSER=webkit is supported."""

import functools
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
import os
from pathlib import Path
import re
import threading
import unittest

from playwright.sync_api import expect, sync_playwright

ROOT = Path(__file__).resolve().parent.parent
ELM_SAVE = "petal-garden-elm-save-v1"
JS_SAVE = "petal-garden-save-v1"


class QuietHandler(SimpleHTTPRequestHandler):
    def log_message(self, *_args):
        pass


class ElmGardenTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.server = ThreadingHTTPServer(
            ("127.0.0.1", 0), functools.partial(QuietHandler, directory=str(ROOT))
        )
        cls.thread = threading.Thread(target=cls.server.serve_forever, daemon=True)
        cls.thread.start()
        cls.url = f"http://127.0.0.1:{cls.server.server_port}/"
        cls.playwright = sync_playwright().start()
        cls.browser = getattr(
            cls.playwright, os.environ.get("GARDEN_BROWSER", "chromium")
        ).launch()

    @classmethod
    def tearDownClass(cls):
        cls.browser.close()
        cls.playwright.stop()
        cls.server.shutdown()
        cls.server.server_close()
        cls.thread.join()

    def setUp(self):
        self.context = self.browser.new_context()
        self.page = self.context.new_page()
        self.errors = []
        self.page.on("pageerror", lambda error: self.errors.append(str(error)))

    def tearDown(self):
        self.context.close()
        self.assertEqual(self.errors, [])

    def seed(self, setup="", key=ELM_SAVE):
        self.page.goto(self.url + "elm/tests.html")
        state = self.page.evaluate(
            """setup => {
                const game = {state: {
                    version: 1, board: Array(49).fill(null),
                    queue: Array.from({length: 5}, () =>
                        ({type: "flower", colour: "blue", inner: null})),
                    score: 0, level: 1, remaining: 24, over: false, seed: 12345
                }};
                new Function("game", setup)(game);
                return game.state;
            }""",
            setup,
        )
        self.page.evaluate(
            "([key,state]) => localStorage.setItem(key,JSON.stringify(state))", [key, state]
        )
        self.page.goto(self.url + "elm/")
        return state

    def test_engine_rules_and_save_codecs(self):
        self.page.goto(self.url + "elm/tests.html")
        report = self.page.evaluate(
            """async () => {
                const worker = Elm.TestRunner.init();
                const ask = input => new Promise(resolve => {
                    const handle = result => {
                        worker.ports.response.unsubscribe(handle);
                        resolve(result);
                    };
                    worker.ports.response.subscribe(handle);
                    worker.ports.request.send(input);
                });
                const flower = (colour="blue",inner=null) => ({type:"flower",colour,inner});
                const fixture = (setup, actions) => {
                    const game = {state: {version:1, board:Array(49).fill(null),
                        queue:Array.from({length:5},()=>flower()), score:0,
                        level:1, remaining:24, over:false, seed:12345}};
                    setup(game);
                    return {game:game.state, actions};
                };
                const assert = (ok,message) => {if(!ok) throw Error(message);};
                const match = await ask(fixture(g=>{},[8,9,10]));
                assert(match.game.score===30 && [8,9,10].every(i=>match.game.board[i]===null),
                    "Three flowers should clear and score 30");
                const cascade = await ask(fixture(g=>{
                    g.state.board[8]=flower("blue","rose");
                    g.state.board[9]=flower("blue","rose");
                    g.state.queue[0]=flower("blue","rose");
                },[10]));
                assert(cascade.game.score===90 && cascade.events[0].waves.length===2,
                    "Revealed inner petals should produce a second wave");
                const crossing = await ask(fixture(g=>{
                    [17,23,25,31].forEach(i=>g.state.board[i]=flower());
                },[24]));
                assert(crossing.game.score===50 && crossing.events[0].waves[0].length===5,
                    "A crossing must count its centre once");
                const rowEdge = await ask(fixture(g=>{
                    [5,6].forEach(i=>g.state.board[i]=flower());
                },[7]));
                assert(rowEdge.game.score===0,"A match must not wrap between rows");
                const spade = await ask(fixture(g=>{
                    g.state.queue[0]={type:"spade"};g.state.board[8]=flower("rose","gold");
                },[9,8,8,8,9]));
                assert(!spade.events[0].ok && spade.events[1].selecting
                    && spade.events[2].selecting && spade.selected===null
                    && spade.game.board[8]===null && spade.game.board[9].inner==="gold"
                    && spade.game.remaining===23,"Spade selection and move");
                const butterfly = await ask(fixture(g=>{
                    g.state.queue[0]={type:"butterfly",colour:"gold"};
                    g.state.board[8]=flower("blue","rose");
                },[9,8]));
                assert(!butterfly.events[0].ok && butterfly.game.board[8].colour==="gold"
                    && butterfly.game.board[8].inner==="rose","Butterfly preserves inner petals");
                const over = await ask(fixture(g=>{
                    g.state.board.fill({type:"rock"});g.state.board[8]=null;
                },[8]));
                assert(over.game.over,"Filling the last patch ends a flower turn");
                const rejected = await ask(fixture(g=>{},[-1,49,null]));
                assert(rejected.events.every(e=>!e.ok) && rejected.game.remaining===24,
                    "Invalid moves and skipping flowers must not consume turns");
                for(let level=1;level<=12;level++) {
                    const advanced = await ask(fixture(g=>{
                        g.state.level=level;g.state.remaining=1;
                        g.state.queue=[{type:"spade"}];g.state.board[0]=flower("rose","gold");
                    },[null]));
                    const target=Math.max(12,level+1<=4?12+level*2:18-(level-3));
                    assert(advanced.game.level===level+1 && advanced.game.score===level*100
                        && advanced.game.board[0].inner==="gold"
                        && advanced.game.board.filter(p=>p?.type==="rock").length===target-1,
                        "Level transition must preserve occupied border patches");
                }
                const initial=fixture(g=>{},[]).game;
                const first=await ask({game:initial,actions:[8]});
                const resumed=await ask({game:first.game,actions:[9]});
                const uninterrupted=await ask({game:initial,actions:[8,9]});
                if(JSON.stringify(resumed.game)!==JSON.stringify(uninterrupted.game))
                    throw Error("Random state was not preserved on resume");
                const malformed=[
                    {...initial,version:2}, {...initial,score:-1},
                    {...initial,board:[]}, {...initial,queue:[]},
                    {...initial,over:true}, {...initial,seed:"bad"}
                ];
                for(const game of malformed) {
                    if(!(await ask({game,actions:[]})).error) throw Error("Invalid save accepted");
                }
                return {malformed:malformed.length};
            }"""
        )
        self.assertEqual(report["malformed"], 6)

    def test_play_save_and_separate_import(self):
        original = self.seed(key=JS_SAVE)
        self.page.locator("#import-game").click()
        self.page.get_by_role("button", name="Copy garden", exact=True).click()
        patches = self.page.locator(".patch")
        expect(patches.nth(8)).to_be_enabled()
        for index in [8, 9, 10]:
            patches.nth(index).click()
        expect(self.page.locator("#score")).to_have_text("30")
        self.assertEqual(
            self.page.evaluate("(k)=>JSON.parse(localStorage.getItem(k))", JS_SAVE), original
        )
        self.page.reload()
        expect(self.page.locator("#score")).to_have_text("30")
        self.page.locator("#new-game").click()
        self.page.get_by_role("button", name="Keep playing").click()
        expect(self.page.locator("#score")).to_have_text("30")
        self.page.locator("#new-game").click()
        self.page.get_by_role("button", name="Start fresh").click()
        expect(self.page.locator("#score")).to_have_text("0")
        expect(self.page.locator("#best")).to_have_text("30")
        self.assertEqual(self.page.locator('.patch[aria-label$="Stone"]').count(), 12)

    def test_generated_doubles_increase_after_level_14(self):
        self.page.goto(self.url + "elm/tests.html")
        samples = self.page.evaluate(
            """async () => {
                const worker = Elm.TestRunner.init();
                const sample = level => new Promise(resolve => {
                    const handle = result => {
                        worker.ports.sampleResult.unsubscribe(handle);
                        resolve({...result,level});
                    };
                    worker.ports.sampleResult.subscribe(handle);
                    worker.ports.sampleLevel.send(level);
                });
                const results = [];
                for(const level of [1,2,7,14,15,20,25,28,40]) results.push(await sample(level));
                return results;
            }"""
        )
        expected = {1: 0, 2: .15, 7: .525, 14: .55, 15: .575,
                    20: .70, 25: .825, 28: .90, 40: .90}
        for sample in samples:
            self.assertEqual(sample["flowers"] + sample["tools"], 6000)
            self.assertAlmostEqual(
                sample["doubles"] / sample["flowers"], expected[sample["level"]], delta=.02
            )
            self.assertAlmostEqual(sample["tools"] / 6000, .12, delta=.02)

    def instrument_audio(self):
        self.context.add_init_script("""
            const NativeAudioContext = window.AudioContext;
            window.audioEvents = [];
            window.AudioContext = class extends NativeAudioContext {
                createBufferSource() {
                    return this.observe(super.createBufferSource(), "rustle");
                }
                createOscillator() {
                    return this.observe(super.createOscillator(), "chime");
                }
                observe(source, kind) {
                    const start = source.start.bind(source);
                    const stop = source.stop.bind(source);
                    source.start = time => {
                        window.audioEvents.push({
                            event:"start",kind,time,frequency:source.frequency?.value,
                            wallTime:performance.now()
                        });
                        start(time);
                    };
                    source.stop = time => {
                        window.audioEvents.push({event:"stop",kind,immediate:time===undefined});
                        stop(time);
                    };
                    return source;
                }
            };
        """)

    def sound_on(self):
        self.page.locator("#sound").click()
        self.page.wait_for_function("audioEvents.some(e=>e.event==='start')")
        self.page.evaluate("audioEvents.length=0")

    def test_gardening_sounds_and_level_flourish(self):
        self.instrument_audio()
        self.seed()
        self.sound_on()
        self.page.locator(".patch").nth(8).click()
        self.page.wait_for_function("audioEvents.filter(e=>e.event==='start').length===2")
        self.assertTrue(self.page.evaluate(
            "audioEvents.filter(e=>e.event==='start').every(e=>e.kind==='rustle')"
        ))
        self.page.evaluate("audioEvents.length=0")
        self.page.locator(".patch").nth(8).click()  # Rejected move is silent.
        expect(self.page.locator("#message")).to_contain_text("Choose an empty patch")
        self.assertEqual(self.page.evaluate("audioEvents.length"), 0)
        self.page.locator("#sound").click()
        expect(self.page.locator("#sound")).to_have_attribute("aria-pressed", "false")
        self.assertGreater(self.page.evaluate(
            "audioEvents.filter(e=>e.event==='stop'&&e.immediate).length"
        ), 0)

        self.seed("""
            game.state.board[8]={type:"flower",colour:"blue",inner:"rose"};
            game.state.board[9]={type:"flower",colour:"blue",inner:"rose"};
            game.state.queue[0]={type:"flower",colour:"blue",inner:"rose"};
        """)
        self.sound_on()
        self.page.locator(".patch").nth(10).click()
        expect(self.page.locator("#board")).to_have_attribute("aria-busy", "true")
        for index in [8, 9, 10]:
            expect(self.page.locator(".patch").nth(index)).to_have_attribute(
                "aria-label", re.compile("blue flower, rose centre$")
            )
            expect(self.page.locator(".patch").nth(index)).to_be_disabled()
        self.page.wait_for_function(
            "audioEvents.filter(e=>e.kind==='chime'&&e.event==='start').length===3"
        )
        for index in [8, 9, 10]:
            expect(self.page.locator(".patch").nth(index)).to_have_attribute(
                "aria-label", re.compile(": rose flower$")
            )
        self.page.wait_for_function("audioEvents.filter(e=>e.kind==='chime'&&e.event==='start').length===6")
        notes = self.page.evaluate(
            "audioEvents.filter(e=>e.kind==='chime'&&e.event==='start')"
        )
        self.assertGreater(notes[3]["frequency"], notes[0]["frequency"])
        self.assertGreater(notes[3]["wallTime"] - notes[0]["wallTime"], 350)
        expect(self.page.locator("#board")).to_have_attribute("aria-busy", "false")
        for index in [8, 9, 10]:
            expect(self.page.locator(".patch").nth(index)).to_have_attribute(
                "aria-label", re.compile("Empty patch$")
            )
        expect(self.page.locator("#message")).to_contain_text("2-step cascade")

        # A single pop also chimes, and the completed result is saved mid-animation.
        self.seed("""
            [8,9].forEach(i => game.state.board[i]=
                {type:"flower",colour:"blue",inner:null});
        """)
        self.sound_on()
        self.page.locator(".patch").nth(10).click()
        self.page.wait_for_function(
            "audioEvents.filter(e=>e.kind==='chime'&&e.event==='start').length===3"
        )
        saved = self.page.evaluate("(key)=>JSON.parse(localStorage.getItem(key))", ELM_SAVE)
        self.assertEqual(saved["score"], 30)
        self.assertTrue(all(saved["board"][index] is None for index in [8, 9, 10]))
        self.page.reload()
        expect(self.page.locator("#board")).to_have_attribute("aria-busy", "false")
        expect(self.page.locator("#score")).to_have_text("30")

        # Completing a level must have a flourish even without a match or planting.
        self.seed("game.state.remaining=1;game.state.queue=[{type:'spade'}];")
        self.sound_on()
        self.page.locator("#skip").click()
        expect(self.page.locator("#level")).to_have_text("2")
        self.page.wait_for_function("audioEvents.filter(e=>e.kind==='chime'&&e.event==='start').length===15")
        self.assertEqual(self.page.evaluate(
            "audioEvents.filter(e=>e.kind==='rustle'&&e.event==='start').length"
        ), 0)
        self.page.locator("#sound").click()
        self.assertGreater(self.page.evaluate(
            "audioEvents.filter(e=>e.event==='stop'&&e.immediate).length"
        ), 0)

    def test_butterfly_spade_and_skipped_tool_audio(self):
        self.instrument_audio()
        self.seed("""
            game.state.board[8]={type:"flower",colour:"rose",inner:"gold"};
            game.state.queue[0]={type:"spade"};
            game.state.queue[1]={type:"butterfly",colour:"violet"};
            game.state.queue[2]={type:"spade"};
        """)
        self.sound_on()
        patches = self.page.locator(".patch")
        patches.nth(8).click()
        expect(patches.nth(8)).to_have_attribute("aria-pressed", "true")
        self.assertEqual(self.page.evaluate("audioEvents.length"), 0)
        patches.nth(9).click()
        self.page.wait_for_function("audioEvents.filter(e=>e.event==='start').length===2")
        self.page.evaluate("audioEvents.length=0")
        patches.nth(9).click()
        self.page.wait_for_function("audioEvents.filter(e=>e.event==='start').length===4")
        self.page.evaluate("audioEvents.length=0")
        self.page.locator("#skip").click()
        expect(self.page.locator("#skip")).not_to_be_visible()
        self.assertEqual(self.page.evaluate("audioEvents.length"), 0)

    def test_audio_failure_is_visible(self):
        self.context.add_init_script("""
            window.AudioContext = class {
                constructor() { throw new Error("Audio unavailable"); }
            };
        """)
        self.seed()
        self.page.locator("#sound").click()
        expect(self.page.locator("#storage-warning")).to_contain_text("Audio isn't available")
        expect(self.page.locator("#sound")).to_have_attribute("aria-pressed", "false")
        self.page.locator(".patch").nth(8).click()
        expect(self.page.locator(".patch").nth(8)).to_have_attribute(
            "aria-label", "Row 2, column 2: blue flower"
        )

    def test_muting_cancels_audio_waiting_for_browser_resume(self):
        self.instrument_audio()
        self.context.add_init_script("""
            AudioContext.prototype.resume = function () {
                return new Promise(resolve => { window.releaseAudio = resolve; });
            };
        """)
        self.seed()
        self.page.locator("#sound").click()
        self.page.wait_for_function("typeof releaseAudio==='function'")
        self.page.locator("#sound").click()
        expect(self.page.locator("#sound")).to_have_attribute("aria-pressed", "false")
        self.page.evaluate("""async () => {
            releaseAudio();
            await new Promise(resolve => requestAnimationFrame(resolve));
        }""")
        self.assertEqual(self.page.evaluate("audioEvents.length"), 0)

    def test_audio_resume_failure_is_visible(self):
        self.context.add_init_script("""
            AudioContext.prototype.resume = function () {
                return Promise.reject(new Error("Browser denied audio"));
            };
        """)
        self.seed()
        self.page.locator("#sound").click()
        expect(self.page.locator("#storage-warning")).to_contain_text("Audio isn't available")
        expect(self.page.locator("#sound")).to_have_attribute("aria-pressed", "false")

    def test_existing_level_14_save_advances_without_resetting(self):
        self.seed("""
            game.state.level=14;
            game.state.score=12345;
            game.state.remaining=1;
            game.state.queue=[{type:"flower",colour:"blue",inner:"gold"}];
            game.state.board[8]={type:"flower",colour:"rose",inner:"violet"};
        """)
        expect(self.page.locator("#level")).to_have_text("14")
        expect(self.page.locator("#score")).to_have_text("12345")
        self.page.locator(".patch").nth(24).click()
        expect(self.page.locator("#level")).to_have_text("15")
        expect(self.page.locator("#score")).to_have_text("13745")
        for index, description in [(8, "rose flower, violet centre"),
                                   (24, "blue flower, gold centre")]:
            expect(self.page.locator(".patch").nth(index)).to_have_attribute(
                "aria-label", re.compile(re.escape(description) + "$")
            )
        self.assertEqual(self.page.locator('.patch[aria-label$="Stone"]').count(), 12)
        self.page.reload()
        expect(self.page.locator("#level")).to_have_text("15")
        expect(self.page.locator("#score")).to_have_text("13745")

    def test_tools_keyboard_and_level(self):
        self.seed("""
            game.state.board[8]={type:"flower",colour:"rose",inner:"gold"};
            game.state.queue[0]={type:"spade"};
            game.state.queue[1]={type:"butterfly",colour:"violet"};
        """)
        self.page.locator("#help").click()
        self.page.get_by_role("button", name="Let's plant").click()
        patches = self.page.locator(".patch")
        patches.nth(8).click()
        expect(patches.nth(8)).to_have_attribute("aria-pressed", "true")
        patches.nth(8).press("ArrowRight")
        expect(patches.nth(9)).to_be_focused()
        patches.nth(9).press("Enter")
        expect(patches.nth(9)).to_have_attribute(
            "aria-label", "Row 2, column 3: rose flower, gold centre"
        )
        patches.nth(9).click()
        expect(patches.nth(9)).to_have_attribute(
            "aria-label", "Row 2, column 3: violet flower, gold centre"
        )
        self.seed("game.state.remaining=1;game.state.queue=[{type:'spade'}];")
        self.page.locator("#skip").click()
        expect(self.page.locator("#level")).to_have_text("2")
        self.assertEqual(self.page.locator('.patch[aria-label$="Stone"]').count(), 14)

    def test_game_over_and_restart(self):
        self.seed("game.state.board.fill({type:'rock'});game.state.board[8]=null;")
        self.page.locator(".patch").nth(8).click()
        expect(self.page.locator("#over-dialog")).to_be_visible()
        self.page.get_by_role("button", name="See my garden").click()
        self.page.reload()
        expect(self.page.locator("#over-dialog")).to_be_visible()
        self.page.get_by_role("button", name="Plant again").click()
        expect(self.page.locator("#over-dialog")).not_to_be_visible()
        expect(self.page.locator("#score")).to_have_text("0")

    def test_bad_save_and_storage_blocked(self):
        self.page.goto(self.url + "elm/")
        self.page.evaluate("(k)=>localStorage.setItem(k,'broken')", ELM_SAVE)
        self.page.reload()
        expect(self.page.locator("#storage-warning")).to_contain_text("couldn't be restored")
        self.context.add_init_script("""
            Object.defineProperty(window,"localStorage",{
                get(){throw new DOMException("Blocked","SecurityError");}
            });
        """)
        self.page.reload()
        expect(self.page.locator("#storage-warning")).to_contain_text("Saving is unavailable")
        self.assertEqual(self.page.locator(".patch").count(), 49)

    def test_direct_file_and_phone(self):
        self.page.goto((ROOT / "index.html").as_uri())
        expect(self.page.locator(".patch")).to_have_count(49)
        self.page.goto((ROOT / "elm" / "index.html").as_uri())
        self.assertEqual(self.page.locator(".patch").count(), 49)
        self.assertTrue(self.page.locator("#current svg").is_visible())
        with self.browser.new_context(
            viewport={"width": 390, "height": 844}, is_mobile=True, has_touch=True
        ) as context:
            page = context.new_page()
            page.on("pageerror", lambda error: self.errors.append(str(error)))
            page.goto(self.url + "elm/")
            self.assertTrue(page.evaluate("document.documentElement.scrollWidth<=innerWidth"))
            while page.locator("#skip").is_visible():
                previous_remaining = page.locator("#remaining").inner_text()
                page.locator("#skip").tap()
                expect(page.locator("#remaining")).not_to_have_text(previous_remaining)
            patch = page.locator('.patch[aria-label$="Empty patch"]').first
            patch_id = patch.get_attribute("id")
            patch.tap()
            expect(page.locator("#" + patch_id)).not_to_have_attribute(
                "aria-label", re.compile("Empty patch$")
            )
            page.set_viewport_size({"width": 320, "height": 568})
            self.assertTrue(page.evaluate("document.documentElement.scrollWidth<=innerWidth"))


if __name__ == "__main__":
    unittest.main(verbosity=2)
