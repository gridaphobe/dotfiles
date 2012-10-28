require 'rubygems'
require 'wirble'
require 'irb/completion'
require 'pp'

Wirble.init

IRB.conf[:AUTO_INDENT]=true

def time(times = 1)
  require 'benchmark'
  ret = nil
  Benchmark.bm { |x| x.report { times.times { ret = yield } } }
  ret
end
