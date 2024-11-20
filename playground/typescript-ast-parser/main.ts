import chalk from "chalk"
import { readFileSync } from "fs"
import * as ts from "typescript"

const syntaxNameOfKind = (kind: ts.SyntaxKind) => ts.SyntaxKind[kind]

type AST = {
	kind: string
	source: string
	children?: AST[]
}

const walk = (node: ts.Node): AST => {
	const children = node.getChildren().map(walk)
	return { kind: syntaxNameOfKind(node.kind), source: node.getText(), children }
}

const printAST = (tree: AST, indent = 0) => {
	const { kind, source, children } = tree
	const isLeaf = !children?.length
	if (isLeaf) {
		console.log("\t".repeat(indent), `${chalk.yellow(kind)}: ${source}`)
	} else {
		console.log("\t".repeat(indent), `[${chalk.cyan(kind)}:`)
		children?.forEach((child) => printAST(child, indent + 1))
		console.log("\t".repeat(indent), "]")
	}
}

const fileNames = process.argv.slice(2)

fileNames.forEach((fileName) => {
	const sourceFile = ts.createSourceFile(
		fileName,
		readFileSync(fileName).toString(),
		ts.ScriptTarget.ESNext,
		/*setParentNodes */ true
	)
	const walks = sourceFile.statements.map(walk)
	walks.forEach((walk) => printAST(walk))
})
