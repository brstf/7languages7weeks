class Tree
	attr_accessor :children, :node_name, :level

	def initialize(hash_tree, level=0)
		@level = level
		@node_name = hash_tree.keys.first
		h_children = hash_tree[node_name]
		@children = h_children.collect {|c|	Tree.new({c[0] => c[1]}, self.level + 1)}
	end

	def visit_all(&block)
		visit &block
		children.each {|c| c.visit_all &block}
	end

	def visit(&block)
		block.call self
	end
end

ruby_tree = Tree.new( {"Ruby" => {"Reia" => {}, "MacRuby" => {}}})
family = {'grandpa' => {'dad' => {'child 1' => {}, 'child 2' => {}}, 'uncle' => {'child 3' => {}, 'child 4' => {}}}}

puts "Visiting a node"
ruby_tree.visit {|node| puts node.node_name}
puts

puts "Visiting entire tree"
ruby_tree.visit_all {|node| puts node.node_name}

family_tree = Tree.new(family)
puts "Family Tree:"
family_tree.visit_all {|node| puts " " * node.level + node.node_name.to_s}