class CsvRow
	def method_missing name, *args
		contents[headers.index(name.to_s)] if headers.index(name.to_s)
	end

	attr_accessor :headers, :contents

	def initialize(headers, contents)
		@headers = headers
		@contents = contents
	end
end

module ActsAsCsv
	def self.included(base)
		base.extend ClassMethods
	end

	module ClassMethods
		def acts_as_csv
			include InstanceMethods
		end
	end

	module InstanceMethods
		def read
			@csv_contents = []
			filename = self.class.to_s.downcase + '.txt'
			puts filename
			file = File.new(filename)
			@headers = file.gets.chomp.split(', ')

			file.each do |row|
				@csv_contents << row.chomp.split(', ')
			end
		end

		def each 
			csv_contents.each {|row| yield CsvRow.new(headers, row)}
		end

		attr_accessor :headers, :csv_contents
		def initialize
			read
		end
	end
end

class RubyCsv
	include ActsAsCsv
	acts_as_csv
end

csv = RubyCsv.new
puts csv.headers.inspect
puts csv.csv_contents.inspect
csv.each {|row| puts row.one}