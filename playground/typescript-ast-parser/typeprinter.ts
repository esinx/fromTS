import chalk from "chalk"
import * as ts from "typescript"

function printTypes(fileNames: string[]) {
	const program = ts.createProgram(fileNames, {})
	const typeChecker = program.getTypeChecker()
	const sources = program
		.getSourceFiles()
		.filter((source) => !source.isDeclarationFile)
	function printNode(node: ts.Node, indent = 0) {
		if (ts.isVariableDeclaration(node)) {
			const type = typeChecker.getTypeAtLocation(node)
			const symbol = typeChecker.getSymbolAtLocation(node.name)
			const name = symbol?.getName()
			const typeString = typeChecker.typeToString(type)
			console.log(chalk.green(name) + ": " + chalk.blue(typeString))
		}
		ts.forEachChild(node, (child) => printNode(child, indent + 2))
	}
	for (const sourceFile of sources) {
		console.log(chalk.red(sourceFile.fileName))
		printNode(sourceFile)
	}
}

const fileNames = process.argv.slice(2)
printTypes(fileNames)
