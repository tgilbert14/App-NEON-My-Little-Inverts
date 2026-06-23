/* =========================================================================
   app.js — count-up stat counters + celebratory confetti + loading overlay +
   the multi-frame map "kick" (re-fit a Leaflet map that initialised hidden).
   Ported from the Mosquito Pulse v2 chrome; behaviour identical.
   ========================================================================= */

// When a tab becomes visible, nudge a resize so widgets that rendered while the
// tab was hidden (Leaflet maps, plotly charts) re-fit to their real size — the
// classic "0-sized widget in a hidden bootstrap tab" fix.
document.addEventListener("shown.bs.tab", function () {
  setTimeout(function () { window.dispatchEvent(new Event("resize")); }, 60);
});

// ---- animated count-up for the hero stat band ----------------------------
function animateCount(el) {
  if (el.dataset.animated === "1") return;
  el.dataset.animated = "1";
  // A freshly-rendered hero counter means a site just finished loading — the
  // most reliable signal to dismiss the loading overlay.
  if (typeof smtLoadDone === "function") smtLoadDone();
  const target = parseFloat(el.getAttribute("data-target")) || 0;
  const suffix = el.dataset.suffix || "";
  const isFloat = !Number.isInteger(target);
  const fmt = (v) => (isFloat ? v.toFixed(1) : Math.round(v).toLocaleString()) + suffix;
  if (window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches) {
    el.textContent = fmt(target); return;
  }
  const dur = 900;
  const start = performance.now();
  function tick(now) {
    const t = Math.min(1, (now - start) / dur);
    const eased = 1 - Math.pow(1 - t, 3); // easeOutCubic
    el.textContent = fmt(target * eased);
    if (t < 1) requestAnimationFrame(tick);
    else el.textContent = fmt(target);
  }
  requestAnimationFrame(tick);
}
function runCounters() {
  document.querySelectorAll(".count-up").forEach(animateCount);
}
const heroObserver = new MutationObserver(() => runCounters());
// Attach immediately when possible (this file can execute AFTER DOMContentLoaded,
// in which case a DOMContentLoaded listener would never fire). document.body is
// available by the time a head-loaded deferred-dependency script runs.
function invObserveCounters() {
  if (document.body) { heroObserver.observe(document.body, { childList: true, subtree: true }); runCounters(); }
  else document.addEventListener("DOMContentLoaded", invObserveCounters);
}
invObserveCounters();

// ---- confetti on a standout (a top EPT site, etc.) -----------------------
function rodentConfetti(big) {
  if (typeof confetti !== "function") return;
  // Riffle & Teal palette (teal / aqua / green / amber / ink).
  const colors = ["#0e8f9c", "#2bb7c4", "#3f9e6e", "#e08a2b", "#102a33"];
  const burst = (opts) => confetti(Object.assign({ colors, disableForReducedMotion: true }, opts));
  burst({ particleCount: big ? 140 : 70, spread: big ? 100 : 70, origin: { y: 0.3 }, startVelocity: 42 });
  if (big) {
    setTimeout(() => burst({ particleCount: 80, angle: 60, spread: 70, origin: { x: 0 } }), 180);
    setTimeout(() => burst({ particleCount: 80, angle: 120, spread: 70, origin: { x: 1 } }), 320);
  }
  mascotCheer(big);
}
function mascotCheer(big) {
  try {
    if (window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches) return;
    var src = document.querySelector("#loadOverlay .mascot");
    if (!src) return;
    var wrap = document.createElement("div");
    wrap.className = "mascot-cheer";
    wrap.appendChild(src.cloneNode(true));
    document.body.appendChild(wrap);
    setTimeout(function () { if (wrap.parentNode) wrap.parentNode.removeChild(wrap); }, 1700);
  } catch (e) {}
}

// ---- first-visit: the splash mascot waves hello once (localStorage-gated) ----
document.addEventListener("DOMContentLoaded", function () {
  try {
    if (localStorage.getItem("invMascotSeen") === "1") return;
    var g = document.querySelector(".splash-guide");
    if (g) {
      g.classList.add("wave");
      localStorage.setItem("invMascotSeen", "1");
      setTimeout(function () { g.classList.remove("wave"); }, 3300);
    }
  } catch (e) {}
});

// ---- restore-last-site + recents (ONE localStorage namespace) --------------
// On (re)connect, hand the server the saved last-site code and the recents ring
// so the one-shot startup resolver and the recents strip can read them. Same
// localStorage round-trip the first-visit mascot uses above. Keys:
//   invLastSite  : the single most-recent site code (the resume target)
//   invRecents   : a comma-joined ring of the last few codes (newest first)
// Both are WRITTEN in one place — the 'invSaveSite' handler below — so there is
// a single source of truth for the persisted state.
function invSendStored() {
  if (!window.Shiny || !Shiny.setInputValue) return;
  try { Shiny.setInputValue("invLastSite", localStorage.getItem("invLastSite") || "", { priority: "event" }); }
  catch (e) { Shiny.setInputValue("invLastSite", "", { priority: "event" }); }
  try { Shiny.setInputValue("invRecents", localStorage.getItem("invRecents") || "", { priority: "event" }); }
  catch (e) { Shiny.setInputValue("invRecents", "", { priority: "event" }); }
}
// Shiny dispatches shiny:connected as a JQUERY event (a native addEventListener
// does NOT catch it). Bind through jQuery, and keep the handler trivial + fully
// guarded: a throw inside a shiny:connected handler bound before shiny.js's own
// can abort Shiny's connect wiring (the input channel never gets set up). The
// try/catch keeps that from ever happening.
if (window.jQuery) jQuery(document).on("shiny:connected", function () {
  try { invSendStored(); } catch (e) {}
});

// ---- loading overlay (opaque, indeterminate) -----------------------------
var smtSafetyTimer = null;
function smtLoadStart(label) {
  var ov = document.getElementById("loadOverlay");
  if (!ov) return;
  var siteText = label || "";
  if (!siteText) {
    var sel = document.getElementById("site");
    if (sel && sel.options && sel.selectedIndex >= 0) siteText = sel.options[sel.selectedIndex].text;
  }
  var siteEl = document.getElementById("loadSite");
  if (siteEl) siteEl.textContent = siteText;
  ov.style.display = "flex";
  if (navigator.vibrate) { try { navigator.vibrate(12); } catch (e) {} }
  clearTimeout(smtSafetyTimer);
  smtSafetyTimer = setTimeout(function () {
    var note = document.querySelector(".load-note");
    if (note) note.textContent = "Still working — a large site can take a moment. You can close this and try again.";
    setTimeout(smtLoadDone, 5000);
  }, 90000);
}
function smtLoadDone() {
  clearTimeout(smtSafetyTimer);
  var ov = document.getElementById("loadOverlay");
  if (ov) ov.style.display = "none";
}

// ---- dismiss any open info popover (click-outside + Esc) -----------------
function smtClosePopovers() {
  document.querySelectorAll(".popover").forEach(function (pop) {
    var trig = pop.id ? document.querySelector('[aria-describedby="' + pop.id + '"]') : null;
    if (trig && window.bootstrap && bootstrap.Popover) {
      var inst = bootstrap.Popover.getInstance(trig);
      if (inst) { inst.hide(); return; }
    }
    pop.remove();
  });
}
document.addEventListener("click", function (e) {
  if (e.target.closest(".popover") || e.target.closest(".info-dot") ||
      e.target.closest("bslib-popover")) return;
  if (document.querySelector(".popover")) smtClosePopovers();
});
document.addEventListener("keydown", function (e) {
  if (e.key === "Escape") smtClosePopovers();
});

// ---- Shiny custom message handlers ---------------------------------------
// Register the Shiny custom-message handlers. IMPORTANT: do NOT gate this on
// DOMContentLoaded — htmlwidget/bslib dependency scripts can push this file's
// execution past the DOMContentLoaded event, in which case a
// `addEventListener("DOMContentLoaded", …)` callback never fires and EVERY
// handler below would silently fail to register (this is exactly the bug that
// left invSaveSite/confetti dead). Instead register as soon as `Shiny` exists,
// polling briefly until it does.
// Shiny's addCustomMessageHandler THROWS if a name is already registered, which
// would abort the whole batch (leaving later handlers — invSaveSite, kickMaps —
// unregistered). Guard each one so a duplicate is a no-op, and so the registration
// is safe to run more than once across init events.
function invAddH(name, fn) {
  if (!window.Shiny || !Shiny.addCustomMessageHandler) return;
  try { Shiny.addCustomMessageHandler(name, fn); } catch (e) { /* already registered */ }
}
function invRegisterHandlers() {
  if (!window.Shiny || !Shiny.addCustomMessageHandler) return false;
  window.__invHandlers = true;     // marker (not a guard — invAddH is per-handler idempotent)
  {
    invAddH("countUp", function () {
      setTimeout(runCounters, 60);
    });
    invAddH("confetti", function (msg) {
      rodentConfetti(msg && msg.big);
    });
    // Persist the loaded site: writes BOTH localStorage keys (last-site + the
    // recents ring) in this ONE place. The server fires it on every successful
    // load; never on the splash. The ring keeps the last 4 distinct codes,
    // newest first, the loaded code promoted to the front.
    invAddH("invSaveSite", function (msg) {
      try {
        var code = msg && msg.site;
        if (!code) return;
        localStorage.setItem("invLastSite", code);
        var raw = localStorage.getItem("invRecents") || "";
        var ring = raw.split(",").map(function (s) { return s.trim(); })
                      .filter(function (s) { return s.length; });
        ring = ring.filter(function (s) { return s !== code; }); // de-dupe
        ring.unshift(code);                                      // newest first
        ring = ring.slice(0, 4);                                 // cap at 4
        localStorage.setItem("invRecents", ring.join(","));
        // hand the refreshed ring back so the splash strip re-renders live
        Shiny.setInputValue("invRecents", ring.join(","), { priority: "event" });
      } catch (e) {}
    });
    invAddH("loadDone", function () { smtLoadDone(); });
    invAddH("smtLoadStart", function (msg) {
      smtLoadStart(msg && msg.label);
    });
    // A Leaflet map that initialised inside a hidden container (the within-site
    // Map tab, or the picker map re-shown after "change site") can paint blank
    // until it recomputes its size. Dispatching 'resize' over several frames
    // makes every Leaflet map invalidateSize. The server kicks this after
    // re-showing the splash and on relevant tab shows.
    invAddH("kickMaps", function () {
      var kick = function () {
        try { window.dispatchEvent(new Event("resize")); } catch (e) {}
        try {
          if (window.HTMLWidgets && HTMLWidgets.findAll) {
            HTMLWidgets.findAll(".leaflet").forEach(function (w) {
              if (w.getMap && w.getMap()) w.getMap().invalidateSize();
            });
          }
        } catch (e) {}
      };
      requestAnimationFrame(kick);
      [80, 250, 500, 900].forEach(function (t) { setTimeout(kick, t); });
    });
  }
  return true;
}
// Register the custom-message handlers. Learned the hard way:
//   1) DO NOT bind invRegisterHandlers to shiny:connected — a handler bound there
//      (before shiny.js binds its own) that runs first can abort Shiny's connect
//      wiring, stalling the whole input channel (no shiny:busy/idle ever fires).
//   2) A pre-connect / top-level registration is DROPPED: Shiny (re)initialises
//      its handler registry during connection, so anything added before the
//      session settles never receives messages.
//   3) Shiny's addCustomMessageHandler THROWS on a duplicate name, so re-running
//      registration must be per-handler guarded (invAddH) or the batch aborts.
// Robust recipe: register only AFTER init, on shiny:sessioninitialized and on the
// first shiny:idle (registry settled by then; not in the connect-wiring chain).
// invAddH makes each call idempotent, so running it on both events is safe and the
// later run is what actually sticks onto the settled registry.
if (window.jQuery) {
  jQuery(document).on("shiny:sessioninitialized", invRegisterHandlers);
  jQuery(document).one("shiny:idle", invRegisterHandlers);
}
