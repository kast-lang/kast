# Kast

An experimental programming language

<!-- kast highlight --html examples/hello.ks -->
<style>
    /*fg colors*/
    code .fg-none    { }
    code .fg-white   { color: #ABB2BF; }
    code .fg-grey    { color: #5C6370; }
    code .fg-green   { color: #98C379; }
    code .fg-yellow  { color: #E5C07B; }
    code .fg-purple  { color: #C678DD; }
    code .fg-magenta { color: #C678DD; }

    code .fg-blue { color: #61AFEF; }
    code .fg-red { color: #E06C75; }
    code .fg-gold { color: #D19A66; }
    code .fg-cyan { color: #56B6C2; }
    code .fg-black { color: #282C34; }
    code .fg-light-black { color: #2C323C; }
    code .fg-gray { color: #3E4452; }
    code .fg-faint-gray { color: #3B4048; }
    code .fg-light-gray { color: #5C6370; }
    code .fg-linenr { color: #4B5263; }

    /*bg colors*/
    code .bg-black   { background-color: #282C34; }
    pre.bg-black     { background-color: #282C34; }

    /*decoration*/
    code .font-italic   { font-style: italic; }
</style>

<pre class="bg-black"><code><span class="fg-magenta">use</span><span class="fg-none"> </span><span class="fg-white">std</span><span class="fg-magenta">.*</span><span class="fg-magenta">;</span><span class="fg-none">
</span><span class="fg-white">print</span><span class="fg-none"> </span><span class="fg-green">"hello"</span><span class="fg-magenta">;</span><span class="fg-none">
</span><span class="fg-green">"world"</span><span class="fg-none"> </span><span class="fg-magenta">|></span><span class="fg-none"> </span><span class="fg-white">print</span><span class="fg-magenta">;</span>
</code></pre>

This is a rewrite #3. You can find previous versions in

1. [`old-ocaml-version`](https://github.com/kast-lang/kast/tree/old-ocaml-version) branch
2. [`old-rust-version`](https://github.com/kast-lang/kast/tree/old-rust-version) branch
3. current version

**NOT READY YET**

[See more on the website](https://kast-lang.org)

![](dep-graph.png)